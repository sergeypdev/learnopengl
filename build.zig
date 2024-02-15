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

    const basisu_optimize = b.option(std.builtin.OptimizeMode, "basisu_optimize", "Optimization level for basisu. ReleaseSafe or faster is recommented, otherwise it's slow or can crash due to ubsan.") orelse .ReleaseFast;

    const basisu_dep = b.dependency("mach-basisu", .{
        .target = target,
        .optimize = basisu_optimize,
    });

    const zalgebra_dep = b.dependency("zalgebra", .{});

    const assets_mod = b.addModule("assets", .{ .root_source_file = .{ .path = "src/assets/root.zig" } });
    const asset_manifest_mod = b.addModule("asset_manifest", .{ .root_source_file = .{ .path = "src/gen/asset_manifest.zig" } });
    asset_manifest_mod.addImport("assets", assets_mod);

    const assets_step = b.step("assets", "Build and install assets");
    b.getInstallStep().dependOn(assets_step);

    const assetc = buildAssetCompiler(b, basisu_optimize, assets_step, buildOptimize);

    assetc.root_module.addImport("assets", assets_mod);
    assetc.root_module.addImport("asset_manifest", asset_manifest_mod);

    const gen_asset_manifest = buildAssets(b, assets_step, assetc, "assets") catch |err| {
        std.log.err("Failed to build assets {}\n", .{err});
        @panic("buildAssets");
    };
    const gen_asset_manifest_mod = b.createModule(.{ .root_source_file = gen_asset_manifest });
    gen_asset_manifest_mod.addImport("assets", assets_mod);
    asset_manifest_mod.addImport("asset_manifest_gen", gen_asset_manifest_mod);

    const lib = b.addSharedLibrary(.{
        .name = "learnopengl",
        .root_source_file = .{ .path = "src/game.zig" },
        .target = target,
        .optimize = optimize,
    });
    const lib_unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/game.zig" },
        .target = target,
        .optimize = optimize,
    });
    const lib_compiles = [_]*Step.Compile{ lib, lib_unit_tests };

    inline for (lib_compiles) |l| {
        l.root_module.addImport("zalgebra", zalgebra_dep.module("zalgebra"));
        l.root_module.addImport("assets", assets_mod);
        l.root_module.addImport("asset_manifest", asset_manifest_mod);
        l.linkLibrary(basisu_dep.artifact("mach-basisu"));
        l.root_module.addImport("mach-basisu", basisu_dep.module("mach-basisu"));
    }

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
        exe.linkSystemLibrary("SDL2");
        inline for (lib_compiles) |l| {
            l.linkSystemLibrary("SDL2");
        }
    } else {
        const sdl_dep = b.dependency("SDL", .{
            .target = target,
            .optimize = .ReleaseSafe,
        });

        const sdl2 = sdl_dep.artifact("SDL2");
        b.getInstallStep().dependOn(&b.addInstallArtifact(sdl2, .{ .dest_dir = .{ .override = .prefix } }).step);
        exe.linkLibrary(sdl2);

        inline for (lib_compiles) |l| {
            l.linkLibrary(sdl2);
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

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

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
    test_step.dependOn(&run_lib_unit_tests.step);
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
fn buildAssets(b: *std.Build, step: *Step, assetc: *Step.Compile, path: []const u8) !Build.LazyPath {
    const assetsPath = b.pathFromRoot(path);
    defer b.allocator.free(assetsPath);
    var assetsDir = try std.fs.openDirAbsolute(assetsPath, .{ .iterate = true });
    defer assetsDir.close();

    var asset_id: usize = 1; // Start at 1 because asset id 0 = null asset
    var meshes = NestedAssetDef{ .path = .{} };
    var shaders = NestedAssetDef{ .path = .{} };
    var shader_programs = NestedAssetDef{ .path = .{} };
    var textures = NestedAssetDef{ .path = .{} };
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

        if (std.mem.endsWith(u8, entry.basename, ".png")) {
            const run_assetc = b.addRunArtifact(assetc);
            run_assetc.addFileArg(.{ .path = b.pathJoin(&.{ path, entry.path }) });
            const out_name = try std.mem.concat(
                b.allocator,
                u8,
                &.{ std.fs.path.stem(entry.basename), ".bu" }, // basisu
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
                try textures.put(b.allocator, out_path, id);
                try asset_paths.append(out_path);
            }
        }
    }

    const manifest_path = try writeAssetManifest(b, asset_paths.items, &meshes, &shaders, &shader_programs, &textures);
    return manifest_path;
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
    asset_paths: [][]const u8,
    meshes: *NestedAssetDef,
    shaders: *NestedAssetDef,
    shader_programs: *NestedAssetDef,
    textures: *NestedAssetDef,
) !Build.LazyPath {
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
    try writeNestedAssetDef(writer, "Texture", "Textures", textures, 0);
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
    const manifest_path = write_step.add("asset_manifest.gen.zig", result);
    return manifest_path;
}

fn buildAssetCompiler(b: *Build, basisu_optimize: std.builtin.OptimizeMode, assets_step: *Step, optimize: std.builtin.OptimizeMode) *Step.Compile {
    const assimp_dep = b.dependency("zig-assimp", .{
        .target = b.host,
        .optimize = optimize,
        //.formats = @as([]const u8, "3DS,3MF,AC,AMF,ASE,Assbin,Assjson,Assxml,B3D,Blender,BVH,C4D,COB,Collada,CSM,DXF,FBX,glTF,glTF2,HMP,IFC,Irr,LWO,LWS,M3D,MD2,MD3,MD5,MDC,MDL,MMD,MS3D,NDO,NFF,Obj,OFF,Ogre,OpenGEX,Ply,Q3BSP,Q3D,Raw,SIB,SMD,Step,STEPParser,STL,Terragen,Unreal,X,X3D,XGL"),
        .formats = @as([]const u8, "Obj"),
    });

    const basisu_dep = b.dependency("mach-basisu", .{
        .target = b.host,
        .optimize = basisu_optimize,
    });

    const assimp_lib = assimp_dep.artifact("assimp");
    const basisu_lib = basisu_dep.artifact("mach-basisu");

    const assetc = b.addExecutable(.{
        .name = "assetc",
        .target = b.host,
        .root_source_file = .{ .path = "tools/asset_compiler.zig" },
        .optimize = optimize,
    });

    assetc.root_module.addAnonymousImport("formats", .{ .root_source_file = .{ .path = "src/formats.zig" } });
    assetc.root_module.addImport("mach-basisu", basisu_dep.module("mach-basisu"));

    assetc.linkLibrary(assimp_lib);
    assetc.linkLibrary(basisu_lib);
    assetc.linkLibC();
    assetc.linkLibCpp();

    assetc.addCSourceFile(.{ .file = .{ .path = "tools/stb/stb_image.c" }, .flags = &.{"-std=c99"} });
    assetc.addIncludePath(.{ .path = "tools/stb" });

    const install_step = b.addInstallArtifact(assetc, .{ .dest_dir = .{ .override = .prefix } });
    assets_step.dependOn(&install_step.step);
    return assetc;
}
