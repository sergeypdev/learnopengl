const std = @import("std");

pub fn getFileModifiedRelative(path: []const u8) !i128 {
    var lib_file = try std.fs.cwd().openFile(path, .{});
    defer lib_file.close();
    var lib_file_meta = try lib_file.metadata();

    return lib_file_meta.modified();
}

pub fn getFileModified(path: []const u8) !i128 {
    var lib_file = try std.fs.openFileAbsolute(path, .{});
    defer lib_file.close();
    var lib_file_meta = try lib_file.metadata();

    return lib_file_meta.modified();
}

pub fn getFileModifiedZ(path: [:0]const u8) !i128 {
    var lib_file = try std.fs.openFileAbsoluteZ(path, .{});
    defer lib_file.close();
    var lib_file_meta = try lib_file.metadata();

    return lib_file_meta.modified();
}
