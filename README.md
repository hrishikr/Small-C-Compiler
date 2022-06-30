# Small-C Compiler

This project simulates a compiler for Small-C (a subset of C language) by implementing a parser, lexer and interpreter. Small-C supports variables, control flow, comparison, math and boolean operations, printing to the console, and maintains type-safety and is Turing complete.

The lexer functions convert a SmallC program internally stored as string into a list of tokens, and the parser functions consume these tokens to produce an abstract symbol tree (AST). The input could be an entire program, or a fragment of a program, and the parser will produce an AST that represents one or more statements (stmt) or an expression (expr). The interpreter then uses this AST to evaluate the code and outputs the results of any print statements present in the source file.

More information in the spec sheet.
