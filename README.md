# Dusk Programming Language

A minimal programming language for game development.

It features a precise, (manually triggered) stop-the-world, mark-and-sweep garbage collector.

Since Dusk is still in the early stages of development, this README is very incomplete and is maintained primarily to serve as personal documentation, rather than to provide external guidance.

## 1. Compiler Usage

### 1.1 Compiler Options

TODO: Keep up-to-date with actual Dusk compilation options.

dusk <directory>
- -o Name of the output executable.
- -w Sets compilation to compile with mingw and target Windows.
- -t Specifies a target architexture for the executable (other than the default).
- -r The directory in which the compiler's runtime is stored (if not the default).
- -help Display a list of compilation options.

### 1.2 Build Instructions

The Dusk language compiler is developed in Ocaml with the Dune build tool, using a simple Makefile as a wrapped around Dusk.

To compile dusk, simply call `make` or `make build`.

## 2. Language Definition

### 2.1 Datatypes

## 3. Technical Design

### 3.1 Compilation Pipeline

- Lexing/Parsing: Source Text [Folder Structure] => Raw AST [Module Structure] (Incremental)
- Resolver: Raw AST [Module Structure] => AST w/ Canonical Names [Flat Structure] (Incremental)
-- Canon AST w/ Module Structure is technically used bc of the incremental structure.
- Type Checker: Canon AST => AST w/ Type Annotations (Incremental)
- Code Generation: Typed AST => Raw ASM
- Linker: Raw ASM => Executable

