const std = @import("std");
const Lexer = @import("lex.zig").Lexer;

pub fn main() !void {
    if (std.os.argv.len != 2) {
        std.debug.print("Usage: {s} <SOURCE_FILE>\n", .{std.os.argv[0]});
        return;
    }

    const filename = std.mem.span(std.os.argv[1]);

    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const contents = std.fs.cwd().readFileAlloc(filename, allocator, .unlimited) catch {
        std.debug.print("failed to read \"{s}\", exiting\n", .{filename});
        return;
    };

    defer allocator.free(contents);

    std.debug.print("Source: {s}\n", .{contents});

    var lexer = Lexer.init(allocator, contents, filename);
    defer lexer.deinit();

    lexer.tokenize() catch |err| {
        std.debug.print("failed to tokenize input: {any}\n", .{err});
        return;
    };

    lexer.log();
}
