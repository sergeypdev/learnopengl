const std = @import("std");
const types = @import("types.zig");
const AssetType = types.AssetType;
const AssetPath = types.AssetPath;

pub const AssetListEntry = struct {
    type: AssetType,
    // original path of the source asset
    src_path: AssetPath,
    // path of the generated processed file
    dst_path: []const u8,

    pub fn getAssetId(self: *const AssetListEntry) u64 {
        return self.src_path.hash();
    }

    // Returns the path that can be used for loading the asset
    // TODO: might be a better way to do this
    pub fn getOutputPath(self: *const AssetListEntry, out_buf: []u8) ![]u8 {
        const path_len = self.src_path.strLen();
        const len = path_len + self.type.ext().len + 1;
        if (out_buf.len < len) {
            return error.BufferTooSmall;
        }

        _ = try self.src_path.toStringSep(out_buf, std.fs.path.sep);

        var end = path_len;
        if (std.mem.lastIndexOf(u8, out_buf[0..path_len], ".")) |ext_idx| {
            end = ext_idx;
        }

        // Extension
        out_buf[end] = '.';
        const end_with_new_ext = end + 1 + self.type.ext().len;
        @memcpy(out_buf[end + 1 .. end_with_new_ext], self.type.ext());

        return out_buf[0..end_with_new_ext];
    }
};

pub fn writeAssetListEntryText(writer: anytype, entry: AssetListEntry) !void {
    try writer.writeAll(@tagName(entry.type));
    try writer.writeByte(' ');
    try entry.src_path.writeString(writer);
    try writer.writeByte(' ');
    try writer.writeAll(entry.dst_path);
}

pub fn readAssetListEntryText(alloc: std.mem.Allocator, reader: anytype) !AssetListEntry {
    var buf: [std.fs.MAX_PATH_BYTES * 2 + 1]u8 = undefined;

    const asset_type_str = try alloc.dupe(u8, try reader.readUntilDelimiterOrEof(&buf, ' ') orelse return error.MissingAssetType);
    const asset_type = std.meta.stringToEnum(AssetType, asset_type_str) orelse return error.InvalidAssetType;
    const src_path_str = try alloc.dupe(u8, try reader.readUntilDelimiterOrEof(&buf, ' ') orelse return error.MissingSrcPath);
    const src_path = AssetPath.fromString(src_path_str);

    const bytes_read = try reader.readAll(&buf);
    const dst_path = try alloc.dupe(u8, buf[0..bytes_read]);

    return AssetListEntry{
        .type = asset_type,
        .src_path = src_path,
        .dst_path = dst_path,
    };
}
