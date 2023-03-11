# The Signature (siglang) programming language project

[Syntax Documentation](SYNTAX.md) | [Contributing Guide](CONTRIBUTING.md)

# Structure

## Frontend

1. **Lexical Analyzer**
    - Lexer (Tokenization)
2. **Syntax Analysis**
    - Parser (Parsing)
3. **Semantic Analysis**
    - `AST >> IR` Compiler, Type Checker (IR Compiling)
4. **Code Generation**
    - `IR >> ByteCode` Compiler (ByteCode Compiling)

## Backend (ByteCode)

1. Optimizer
2. Virtual Machine

Not included in this repository.

see [`siglang/bytecode`](https://github.com/siglang/bytecode) for the bytecode specification.

# Progress Status

-   [x] Lexer, Parser (AST Structure) [[source]](./parser/)
-   [ ] IR Compiler (IR Structure)
-   [ ] Type Checker (in IR Compiler)
-   [ ] ByteCode Compiler
