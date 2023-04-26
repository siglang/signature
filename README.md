# The Signature (siglang) programming language project

[Syntax Documentation](SYNTAX.md) | [Contributing Guide](CONTRIBUTING.md)

# Structure

## Frontend

1. **Lexical Analyzer** (Source Code >> Tokens)
    - Lexer (Tokenization)
2. **Syntax Analysis** (Tokens >> AST)
    - Parser (Parsing)
3. **Semantic Analysis** (AST >> AST)
    - Analyzer (Semantic Analysis), Type Checker (Type Checking)
4. **Code Generation** (AST >> IR)
    - `AST >> ByteCode` Code Generator (ByteCode Generating)

## Backend (ByteCode)

Not included in this repository.

see [`siglang/bytecode`](https://github.com/siglang/bytecode) for the bytecode specification.

# Progress Status

-   [x] Lexer, Parser (AST Structure) [[source]](./parser/)
-   [ ] Semantic Analyzer (Type Checker)
-   [ ] ByteCode Generator
