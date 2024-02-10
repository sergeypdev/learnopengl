const std = @import("std");
const Build = std.Build;
const Step = Build.Step;

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});
    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const buildOptimize = b.option(
        std.builtin.OptimizeMode,
        "boptimize",
        "Prioritize performance, safety, or binary size for build time tools",
    ) orelse .Debug;

    const assets_mod = b.addModule("assets", .{ .root_source_file = .{ .path = "src/assets/root.zig" } });
    const asset_manifest_mod = b.addModule("asset_manifest", .{ .root_source_file = .{ .path = "src/gen/asset_manifest.zig" } });
    asset_manifest_mod.addImport("assets", assets_mod);

    const assets_step = b.step("assets", "Build and install assets");
    b.getInstallStep().dependOn(assets_step);

    const assetc = buildAssetCompiler(b, buildOptimize);
    assetc.root_module.addImport("assets", assets_mod);
    assetc.root_module.addImport("asset_manifest", asset_manifest_mod);

    buildAssets(b, assets_step, assetc, "assets") catch |err| {
        std.log.err("Failed to build assets {}\n", .{err});
        @panic("buildAssets");
    };

    const zalgebra_dep = b.dependency("zalgebra", .{});

    const lib = b.addSharedLibrary(.{
        .name = "learnopengl",
        .root_source_file = .{ .path = "src/game.zig" },
        .target = target,
        .optimize = optimize,
    });

    lib.root_module.addImport("zalgebra", zalgebra_dep.module("zalgebra"));
    lib.root_module.addImport("assets", assets_mod);
    lib.root_module.addImport("asset_manifest", asset_manifest_mod);

    const install_lib = b.addInstallArtifact(lib, .{ .dest_dir = .{ .override = .prefix } });
    b.getInstallStep().dependOn(&install_lib.step);

    const exe = b.addExecutable(.{
        .name = "learnopengl",
        // In this case the main source file is merely a path, however, in more
        // complicated build scripts, this could be a generated file.
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    if (b.systemIntegrationOption("SDL2", .{ .default = b.host.result.os.tag != .windows })) {
        lib.linkSystemLibrary("SDL2");
        exe.linkSystemLibrary("SDL2");
    } else {
        if (b.lazyDependency("SDL", .{
            .target = target,
            .optimize = .ReleaseSafe,
        })) |sdl_dep| {
            const sdl2 = sdl_dep.artifact("SDL2");
            lib.linkLibrary(sdl2);
            exe.linkLibrary(sdl2);
        }
    }

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    const install_exe = b.addInstallArtifact(exe, .{ .dest_dir = .{ .override = .prefix } });
    b.getInstallStep().dependOn(&install_exe.step);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // // Creates a step for unit testing. This only builds the test executable
    // // but does not run it.
    // const lib_unit_tests = b.addTest(.{
    //     .root_source_file = .{ .path = "src/root.zig" },
    //     .target = target,
    //     .optimize = optimize,
    // });

    // const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    // test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);
}

const NestedAssetDef = union(enum) {
    path: std.StringArrayHashMapUnmanaged(NestedAssetDef),
    asset: usize,

    pub fn put(self: *NestedAssetDef, allocator: std.mem.Allocator, path: []const u8, id: usize) !void {
        var iter = try std.fs.path.componentIterator(path);
        const filename = iter.last().?.name;
        _ = iter.first();
        // Skip first one because it's always "assets"
        _ = iter.next();

        var current = &self.path;

        while (iter.next()) |comp| {
            if (comp.name.ptr == filename.ptr) break;
            const gop = try current.getOrPut(allocator, comp.name);
            if (!gop.found_existing) {
                gop.value_ptr.* = NestedAssetDef{ .path = .{} };
            }
            current = &gop.value_ptr.path;
        }

        try current.put(allocator, std.fs.path.stem(filename), NestedAssetDef{ .asset = id });
    }

    pub fn deinit(self: *NestedAssetDef, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .path => |*path| path.deinit(allocator),
            else => {},
        }
    }
};

// Find all assets and cook them using assetc
fn buildAssets(b: *std.Build, step: *Step, assetc: *Step.Compile, path: []const u8) !void {
    const assetsPath = b.pathFromRoot(path);
    defer b.allocator.free(assetsPath);
    var assetsDir = try std.fs.openDirAbsolute(assetsPath, .{ .iterate = true });
    defer assetsDir.close();

    var asset_id: usize = 1; // Start at 1 because asset id 0 = null asset
    var meshes = NestedAssetDef{ .path = .{} };
    var shaders = NestedAssetDef{ .path = .{} };
    var shader_programs = NestedAssetDef{ .path = .{} };
    var asset_paths = std.ArrayList([]const u8).init(b.allocator);

    var walker = try assetsDir.walk(b.allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (std.mem.endsWith(u8, entry.basename, ".obj")) {
            const run_assetc = b.addRunArtifact(assetc);
            run_assetc.addFileArg(.{ .path = b.pathJoin(&.{ path, entry.path }) });
            const out_name = try std.mem.concat(
                b.allocator,
                u8,
                &.{ std.fs.path.stem(entry.basename), ".mesh" },
            );
            const compiled_file = run_assetc.addOutputFileArg(out_name);

            const out_path = b.pathJoin(&.{
                path,
                std.fs.path.dirname(entry.path) orelse "",
                out_name,
            });
            const install_asset = b.addInstallFileWithDir(
                compiled_file,
                .prefix,
                out_path,
            );
            step.dependOn(&install_asset.step);

            {
                const id = asset_id;
                asset_id += 1;
                try meshes.put(b.allocator, out_path, id);
                try asset_paths.append(out_path);
            }
        }

        if (std.mem.endsWith(u8, entry.basename, ".glsl")) {
            const out_path = b.pathJoin(&.{
                path,
                entry.path,
            });
            const install_shader = b.addInstallFileWithDir(.{ .path = out_path }, .prefix, out_path);
            step.dependOn(&install_shader.step);

            {
                const id = asset_id;
                asset_id += 1;
                try shaders.put(b.allocator, out_path, id);
                try asset_paths.append(out_path);
            }
        }

        // Shader program
        if (std.mem.endsWith(u8, entry.basename, ".prog")) {
            const run_assetc = b.addRunArtifact(assetc);
            run_assetc.addFileArg(.{ .path = b.pathJoin(&.{ path, entry.path }) });
            const compiled_file = run_assetc.addOutputFileArg(b.dupe(entry.basename));

            const out_path = b.pathJoin(&.{
                path,
                entry.path,
            });
            const install_asset = b.addInstallFileWithDir(
                compiled_file,
                .prefix,
                out_path,
            );
            step.dependOn(&install_asset.step);

            {
                const id = asset_id;
                asset_id += 1;
                try shader_programs.put(b.allocator, out_path, id);
                try asset_paths.append(out_path);
            }
        }
    }

    const manifest_step = try writeAssetManifest(b, step, asset_paths.items, &meshes, &shaders, &shader_programs);

    assetc.step.dependOn(&manifest_step.step);
}

fn writeNestedAssetDef(writer: anytype, handle: []const u8, name: []const u8, asset_def: *NestedAssetDef, indent: usize) !void {
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
        .asset => |id| {
            try writer.writeByteNTimes(' ', indent * 4);
            try std.fmt.format(writer, "pub const {} = Handle.{s}{{ .id = {} }};\n", .{ std.zig.fmtId(name), handle, id });
        },
    }
}

fn writeAssetManifest(
    b: *Build,
    asset_step: *Step,
    asset_paths: [][]const u8,
    meshes: *NestedAssetDef,
    shaders: *NestedAssetDef,
    shader_programs: *NestedAssetDef,
) !*Step.WriteFile {
    var mesh_asset_manifest = std.ArrayList(u8).init(b.allocator);
    const writer = mesh_asset_manifest.writer();

    try writer.writeAll("// Generated file, do not edit manually!\n\n");
    try writer.writeAll("const std = @import(\"std\");\n");
    // TODO: import AssetId instead of harcoding u32s
    try writer.writeAll("const Handle = @import(\"assets\").Handle;\n\n");

    try writeNestedAssetDef(writer, "Mesh", "Meshes", meshes, 0);
    try writer.writeByte('\n');
    try writeNestedAssetDef(writer, "Shader", "Shaders", shaders, 0);
    try writer.writeByte('\n');
    try writeNestedAssetDef(writer, "ShaderProgram", "ShaderPrograms", shader_programs, 0);
    try writer.writeByte('\n');

    try writer.writeAll("pub const asset_paths = [_][]const u8{\n");
    for (asset_paths) |path| {
        try std.fmt.format(writer, "    \"{}\",\n", .{std.zig.fmtEscapes(path)});
    }
    try writer.writeAll("};\n\n");

    try writer.writeAll("pub const asset_path_to_asset_id = std.ComptimeStringMap(u32, .{\n");
    for (asset_paths, 0..) |path, i| {
        try std.fmt.format(writer, "    .{{ \"{}\", {} }},\n", .{ std.zig.fmtEscapes(path), i + 1 });
    }
    try writer.writeAll("});\n\n");

    const result = mesh_asset_manifest.toOwnedSlice() catch @panic("OOM");
    const write_step = b.addWriteFiles();
    write_step.addBytesToSource(result, "src/gen/asset_manifest.gen.zig");
    asset_step.dependOn(&write_step.step);
    return write_step;
}

fn buildAssetCompiler(b: *Build, optimize: std.builtin.OptimizeMode) *Step.Compile {
    const assimp_dep = b.dependency("zig-assimp", .{
        .target = b.host,
        .optimize = optimize,
        //.formats = @as([]const u8, "3DS,3MF,AC,AMF,ASE,Assbin,Assjson,Assxml,B3D,Blender,BVH,C4D,COB,Collada,CSM,DXF,FBX,glTF,glTF2,HMP,IFC,Irr,LWO,LWS,M3D,MD2,MD3,MD5,MDC,MDL,MMD,MS3D,NDO,NFF,Obj,OFF,Ogre,OpenGEX,Ply,Q3BSP,Q3D,Raw,SIB,SMD,Step,STEPParser,STL,Terragen,Unreal,X,X3D,XGL"),
        .formats = @as([]const u8, "Obj"),
    });
    //const assimp = assimp_dep.builder.dependency("assimp", .{});
    const assimp_lib = assimp_dep.artifact("assimp");

    const assetc = b.addExecutable(.{
        .name = "assetc",
        .target = b.host,
        .root_source_file = .{ .path = "tools/asset_compiler.zig" },
        .optimize = optimize,
    });

    assetc.root_module.addAnonymousImport("formats", .{ .root_source_file = .{ .path = "src/formats.zig" } });

    assetc.linkLibrary(assimp_lib);
    assetc.linkLibC();
    assetc.linkLibCpp();
    b.installArtifact(assetc);
    return assetc;
}
