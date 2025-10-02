const std = @import("std");
const Lexer = @import("lex.zig").Lexer;
const Parser = @import("parse.zig").Parser;
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const IREmitter = @import("ir.zig").IREmitter;

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

    std.debug.print("Source: \n{s}\n", .{contents});

    var lexer = Lexer.init(allocator, contents, filename);
    defer lexer.deinit();

    lexer.tokenize() catch |err| {
        std.debug.print("failed to tokenize input: {any}\n", .{err});
        return;
    };

    //lexer.log();

    var parser = Parser.init(allocator, lexer.tokens.items);
    parser.parse();
    defer parser.deinit();

    //std.debug.print("\nAST:\n", .{});
    //for (parser.program.items) |statement|
    //    std.debug.print("  {any}\n", .{statement});

    var semantic_analyzer = SemanticAnalyzer.init(allocator, parser.program);
    defer semantic_analyzer.deinit();

    std.debug.print("\nSemantics:\n", .{});
    semantic_analyzer.analyze();

    var ir_gen = IREmitter.init(allocator);
    defer ir_gen.deinit();

    ir_gen.emit(parser.program.items);

    std.debug.print("\n\nIR:\n", .{});
    ir_gen.log();
}
