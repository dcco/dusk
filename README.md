# Dusk Programming Language

A minimal programming language for game development.

It features a precise, (manually triggered) stop-the-world, mark-and-sweep garbage collector.

## Compilation Pipeline

- Lexing/Parsing: Source Text [Folder Structure] => Raw AST [Module Structure] (Incremental)
- Resolver: Raw AST [Module Structure] => AST w/ Canonical Names [Flat Structure] (Incremental)
-- Canon AST w/ Module Structure is technically used bc of the incremental structure.
- Type Checker: Canon AST => AST w/ Type Annotations (Incremental)
- Code Generation: Typed AST => Raw ASM
- Linker: Raw ASM => Executable

