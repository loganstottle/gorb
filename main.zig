const std = @import("std");

const Lexer = @import("./front/lex.zig").Lexer;
const Parser = @import("./front/parse.zig").Parser;
const SemanticAnalyzer = @import("./front/semantic.zig").SemanticAnalyzer;
const IREmitter = @import("./mid/ir.zig").IRModule;
const DomTree = @import("./mid/analyze/domtree.zig").DomTree;
const mem2reg = @import("./mid/transform/ssa.zig").mem2reg;
const LLIRModule = @import("./back/x86_64/x86_64.zig").LLIRModule;

pub fn main() !void {
    if (std.os.argv.len != 2) {
        std.debug.print("Usage: {s} <SOURCE_FILE>\n", .{std.os.argv[0]});
        return;
    }

    const filename = std.mem.span(std.os.argv[1]);

    var gpa = std.heap.DebugAllocator(.{}){};
    //defer _ = gpa.deinit();

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

    var semantic_analyzer = SemanticAnalyzer.init(allocator, parser.functions);
    defer semantic_analyzer.deinit();

    std.debug.print("\nSemantics:\n", .{});
    semantic_analyzer.analyze();

    var ir_module = IREmitter.init(allocator);
    defer ir_module.deinit();

    ir_module.emit(parser.functions.items);

    std.debug.print("\nIR:\n", .{});
    ir_module.log();
    for (ir_module.functions.items) |*func| {
        func.dbg();
        func.dot(func.signature.name);
    }

    // var domtree = DomTree.init(allocator, &ir_module.functions.items[0]);
    // domtree.calculate();
    //
    // const m = mem2reg(allocator, &ir_module.functions.items[0], domtree.idom);
    //
    // var iter = m.keyIterator();
    // while (iter.next()) |key| {
    //     if (m.get(key.*)) |phis| {
    //         std.debug.print("{s}:", .{key.*});
    //
    //         var iter2 = phis.keyIterator();
    //         while (iter2.next()) |n| std.debug.print(" {}", .{n.*});
    //
    //         std.debug.print("\n", .{});
    //     }
    // }

    var llir_module = LLIRModule.init(&ir_module);
    llir_module.lower();
    llir_module.log();
}
