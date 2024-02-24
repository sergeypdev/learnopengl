const std = @import("std");
const Build = std.Build;
const Step = Build.Step;
const fs = std.fs;
const asset_list = @import("asset_list.zig");
const AssetListEntry = asset_list.AssetListEntry;
const types = @import("types.zig");

pub const GenerateAssetManifest = @This();
const MAX_ASSET_LIST_BYTES = 1024 * 1024 * 128;

step: Step,
generated_asset_lists: std.ArrayList(Build.LazyPath),
generated_manifest: Build.GeneratedFile,

pub fn create(b: *Build) *GenerateAssetManifest {
    const gam = b.allocator.create(GenerateAssetManifest) catch @panic("OOM");
    gam.* = .{
        .step = Step.init(.{
            .id = .custom,
            .name = "GenerateAssetManifest",
            .owner = b,
            .makeFn = make,
        }),
        .generated_asset_lists = std.ArrayList(Build.LazyPath).init(b.allocator),
        .generated_manifest = Build.GeneratedFile{ .step = &gam.step },
    };

    return gam;
}

pub fn getAssetManifest(self: *const GenerateAssetManifest) Build.LazyPath {
    return Build.LazyPath{ .generated = &self.generated_manifest };
}

pub fn addAssetListFile(self: *GenerateAssetManifest, file: Build.LazyPath) void {
    self.generated_asset_lists.append(file) catch @panic("OOM");
    file.addStepDependencies(&self.step);
}

const AssetMap = std.AutoHashMap(types.AssetType, NestedAssetDef);

fn make(step: *Step, prog_node: *std.Progress.Node) !void {
    _ = prog_node; // autofix
    const b = step.owner;
    var arena = std.heap.ArenaAllocator.init(b.allocator);
    defer arena.deinit();

    const alloc = arena.allocator();
    const self = @fieldParentPtr(GenerateAssetManifest, "step", step);

    var man = b.graph.cache.obtain();
    defer man.deinit();

    // Random bytes to make WriteFile unique. Refresh this with
    // new random bytes when GenerateAssetManifest implementation is modified
    // in a non-backwards-compatible way.
    man.hash.add(@as(u32, 0xd767ee75));

    // TODO: sort generated asset lists to make sure cache is predictable

    for (self.generated_asset_lists.items) |asset_list_file| {
        const file_path = asset_list_file.getPath2(b, step);
        _ = try man.addFile(file_path, null);
    }

    if (try step.cacheHit(&man)) {
        // cache hit, skip running command
        const digest = man.final();

        self.generated_manifest.path = try b.cache_root.join(b.allocator, &.{ "o", &digest, "asset_manifest.gen.zig" });
        step.result_cached = true;
        return;
    }

    const digest = man.final();
    const cache_path = "o" ++ fs.path.sep_str ++ digest;

    var cache_dir = b.cache_root.handle.makeOpenPath(cache_path, .{}) catch |err| {
        return step.fail("unable to make path '{}{s}': {s}", .{
            b.cache_root, cache_path, @errorName(err),
        });
    };
    defer cache_dir.close();

    self.generated_manifest.path = try b.cache_root.join(b.allocator, &.{ "o", &digest, "asset_manifest.gen.zig" });

    // texture assets/bunny_tex.png zig-cache/o/asdjfhlkahsdf/bunny_tex.tex
    // mesh assets/meshes.blend#mesh_01 zig-cache/o/asjdhflksadf/mesh_01.mesh
    var assets = std.ArrayList(AssetListEntry).init(alloc);

    for (self.generated_asset_lists.items) |asset_list_file| {
        const file_path = asset_list_file.getPath2(b, step);

        const asset_list_contents = b.build_root.handle.readFileAlloc(b.allocator, file_path, MAX_ASSET_LIST_BYTES) catch |err| {
            return step.fail("failed to read asset list {s}: {s}", .{ file_path, @errorName(err) });
        };

        var line_iter = std.mem.splitScalar(u8, asset_list_contents, '\n');

        while (line_iter.next()) |line| {
            if (line.len == 0) continue;

            var line_stream = std.io.fixedBufferStream(line);
            const asset_list_entry = try asset_list.readAssetListEntryText(alloc, line_stream.reader());
            std.log.debug("list entry {} {s}", .{ asset_list_entry.type, asset_list_entry.src_path.getPath() });
            try assets.append(asset_list_entry);
        }
    }

    var file = cache_dir.createFile("asset_manifest.gen.zig", .{}) catch |err| {
        return step.fail("unable to create file {}{s}{}{s}: {s}", .{
            b.cache_root, cache_path, std.fs.path.sep, "asset_manifest.gen.zig", @errorName(err),
        });
    };
    defer file.close();

    var writer = std.io.bufferedWriter(file.writer());

    try writeAssetManifest(alloc, writer.writer(), assets.items);
    try writer.flush();

    try step.writeManifest(&man);
}

fn writeAssetManifest(arena: std.mem.Allocator, writer: anytype, assets: []AssetListEntry) !void {
    var asset_map = AssetMap.init(arena);

    for (assets) |asset_list_entry| {
        const gop = try asset_map.getOrPut(asset_list_entry.type);
        if (!gop.found_existing) {
            gop.value_ptr.* = NestedAssetDef{ .path = .{} };
        }

        try gop.value_ptr.put(arena, asset_list_entry.src_path, asset_list_entry);
    }

    // Header
    try writer.writeAll("// Generated file, do not edit manually!\n\n");
    try writer.writeAll("const std = @import(\"std\");\n");
    // TODO: import AssetId instead of harcoding u32s
    try writer.writeAll("const Handle = @import(\"assets\").Handle;\n\n");

    // Asset handles
    var iter = asset_map.iterator();
    while (iter.next()) |entry| {
        try writeNestedAssetDef(writer, @tagName(entry.key_ptr.*), entry.key_ptr.pluralName(), entry.value_ptr, 0);
        try writer.writeByte('\n');
    }

    var buf: [std.os.PATH_MAX]u8 = undefined;

    // TODO: think about building a perfect hashmap
    // AssetId -> Asset path mapping
    try writer.writeAll(
        \\var buf: [1024 * 1024]u8 = undefined;
        \\var fba = std.heap.FixedBufferAllocator.init(&buf);
        \\pub var asset_paths = std.AutoHashMapUnmanaged(u64, []const u8){};
        \\var initialized = false;
        \\
        \\// Fill map with data
        \\pub fn init() void {
        \\    if (initialized) return;
        \\    initialized = true;
        \\
    );
    for (assets) |asset_list_entry| {
        const path = try asset_list_entry.getOutputPath(&buf);
        std.log.debug("asset output path: {s}\n", .{path});
        try std.fmt.format(writer, "    asset_paths.put(fba.allocator(), {}, \"{}\") catch @panic(\"OOM\");\n", .{ asset_list_entry.getAssetId(), std.zig.fmtEscapes(path) });
    }
    try writer.writeAll("}\n\n");

    try writer.writeAll("pub const asset_path_to_asset_id = std.ComptimeStringMap(u64, .{\n");
    for (assets) |asset_list_entry| {
        const path = try asset_list_entry.getOutputPath(&buf);
        try std.fmt.format(writer, "    .{{ \"{}\", {} }},\n", .{
            std.zig.fmtEscapes(path),
            asset_list_entry.getAssetId(),
        });
    }
    try writer.writeAll("});\n\n");
}

const NestedAssetDef = union(enum) {
    path: std.StringArrayHashMapUnmanaged(NestedAssetDef),
    asset: AssetListEntry,

    pub fn put(
        self: *NestedAssetDef,
        allocator: std.mem.Allocator,
        path: types.AssetPath,
        asset_list_entry: AssetListEntry,
    ) !void {
        var iter = try std.fs.path.componentIterator(path.getPath());
        const filename = iter.last().?.name;
        _ = iter.first();

        var current = &self.path;

        while (iter.next()) |comp| {
            if (comp.name.ptr == filename.ptr) break;
            const gop = try current.getOrPut(allocator, comp.name);
            if (!gop.found_existing) {
                gop.value_ptr.* = NestedAssetDef{ .path = .{} };
            }
            current = &gop.value_ptr.path;
        }

        if (path.getSubPath()) |sub_path| {
            const gop = try current.getOrPut(allocator, std.fs.path.stem(filename));
            if (!gop.found_existing) {
                gop.value_ptr.* = NestedAssetDef{ .path = .{} };
            }
            try gop.value_ptr.put(allocator, .{ .simple = sub_path }, asset_list_entry);
        } else {
            try current.put(allocator, std.fs.path.stem(filename), NestedAssetDef{
                .asset = asset_list_entry,
            });
        }
    }

    pub fn deinit(self: *NestedAssetDef, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .path => |*path| path.deinit(allocator),
            else => {},
        }
    }
};

fn writeNestedAssetDef(writer: anytype, handle: []const u8, name: []const u8, asset_def: *const NestedAssetDef, indent: usize) !void {
    switch (asset_def.*) {
        .path => |*path| {
            var iter = path.iterator();

            try writer.writeByteNTimes(' ', indent * 4);
            try std.fmt.format(writer, "pub const {} = struct {{\n", .{std.zig.fmtId(name)});
            while (iter.next()) |entry| {
                try writeNestedAssetDef(writer, handle, entry.key_ptr.*, entry.value_ptr, indent + 1);
            }
            try writer.writeByteNTimes(' ', indent * 4);
            try std.fmt.format(writer, "}};\n", .{});
        },
        .asset => |asset_list_entry| {
            try writer.writeByteNTimes(' ', indent * 4);
            try std.fmt.format(writer, "pub const {} = Handle.{s}{{ .id = {} }};\n", .{
                std.zig.fmtId(name),
                handle,
                asset_list_entry.getAssetId(),
            });
        },
    }
}
