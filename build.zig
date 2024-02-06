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

    const assets_step = b.step("assets", "Build and install assets");
    b.getInstallStep().dependOn(assets_step);

    const assetc = buildAssetCompiler(b, buildOptimize);

    buildAssets(b, assets_step, assetc, "assets") catch |err| {
        std.log.err("Failed to build assets {}\n", .{err});
        @panic("buildAssets");
    };

    const sdl_dep = b.dependency("SDL", .{
        .target = target,
        .optimize = .ReleaseSafe,
    });
    const sdl2 = sdl_dep.artifact("SDL2");

    const zlm_dep = b.dependency("zlm", .{});

    const lib = b.addSharedLibrary(.{
        .name = "learnopengl",
        .root_source_file = .{ .path = "src/game.zig" },
        .target = target,
        .optimize = optimize,
    });

    lib.linkLibrary(sdl2);
    lib.root_module.addImport("zlm", zlm_dep.module("zlm"));

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

    exe.linkLibrary(sdl2);

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

// Find all assets and cook them using assetc
fn buildAssets(b: *std.Build, step: *Step, assetc: *Step.Compile, path: []const u8) !void {
    const assetsPath = b.pathFromRoot(path);
    defer b.allocator.free(assetsPath);
    var assetsDir = try std.fs.openDirAbsolute(assetsPath, .{ .iterate = true });
    defer assetsDir.close();

    var walker = try assetsDir.walk(b.allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (std.mem.eql(u8, ".obj", std.fs.path.extension(entry.basename))) {
            const run_assetc = b.addRunArtifact(assetc);
            run_assetc.addFileArg(.{ .path = b.pathJoin(&.{ path, entry.path }) });
            const out_name = try std.mem.concat(b.allocator, u8, &.{ std.fs.path.stem(entry.basename), ".mesh" });
            const out_file = run_assetc.addOutputFileArg(out_name);

            const out_path = b.pathJoin(&.{
                std.fs.path.dirname(entry.path) orelse ".",
                path,
                out_name,
            });
            const install_asset = b.addInstallFileWithDir(
                out_file,
                .prefix,
                out_path,
            );
            step.dependOn(&install_asset.step);
        }
    }
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
