> **Note**
>
> You are looking at a prototype version of the [**Signature Programming Language**](https://github.com/siglang/signature). (can be changed in the future)
> 
> as of `2023/02/05` Sanetaka (prototype) development and support has been discontinued.

<br/>

# The Sanetaka (sntk) programming language project

[Syntax Documentation](SYNTAX.md) | [Contributing Guide](CONTRIBUTING.md)

> **Warning**
>
> It is still under development and is **not available yet.**

# What features does it have?

We will provide the following features:

-   **Easy and simple, and even beginners can learn it quickly and easily.**
    -   it also has a **C-like** syntax, which makes it easy for other programmers to use the C-like syntax to learn.
-   Supports **functional programming**, **macro** (*it is similar to Rust's Macros, and will provide a powerful macro system*) and **static type system**.
-   **By providing an interpreter plugin**, it can transcompile to other languages. (e.g. JavaScript)
-   **`null` does not exist.** it is in the form of Rust's `Option<T>`, a monadic type will be decalred in the standard library.

**... and more.**

this may not be observed. (most likely.) however, we strive to provide better features.

# Progress

-   [x] Tokenizer (Tokens + Lexer)
-   [x] Parser (AST) **(in progress)**
    -   [ ] EEE (Evaluating an Evaluable Expression) **(in progress)**
-   [x] Compiler (Sanetaka IR) **(in progress)**
    -   [ ] Type Checker **(in progress)**
-   [ ] IR Interpreter **(in progress)**
-   [ ] Plugin (e.g. Transcompiler)
    -   [ ] Plugin API

-   [ ] Macro
-   [ ] Type System **(in progress)**

-   [ ] Standard Library
-   [ ] Package Manager
-   [ ] Documentation (Click [**here**](./SYNTAX.md) to see **syntax documentation**)

> **Note**
>
> This plan is also subject to change during development.

# How does it work?

```mermaid
graph LR
    _[Source Code] --> |sntkc| Lexer
    Lexer[Tokenizer / Lexer] --> Parser[Parser / AST]
    Parser --> Compiler[Sanetaka IR / Compiler]
    Compiler --> Run[sntk run]
    Run --> |Run| VM[Virtual Machine / Interpreter]
    Compiler --> Plugin[plugin]
    Plugin --> Transcompile[Transcompile]
    Transcompile --> TResult[JavaScript, etc..]
```

<details>
    <summary>If <b>mermaid</b> doesn't render, click here. (image)</summary>
    <img src="./resource/mermaid.png" />
</details>
