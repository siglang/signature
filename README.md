## The Sanetaka (sntk) programming language project

> **Warning**
>
> It is still under development and is not available yet.

---

We will provide the following features:

-   **Easy and simple, and even beginners can learn it quickly and easily.**
    -   it also has a C-like syntax, which makes it easy for other programmers to use the C-like syntax to learn.
-   **Using bytecode, it provides a fast interpreter.**
-   **Supports functional programming.**
-   **Supports macro. it is similar to Rust's Macros, and will provide a powerful macro system.**
-   **Supports type system.**
-   **By providing an interpreter plugin, it can transcompile to other languages. (e.g. JavaScript)**

... and more.

this may not be observed. (No, most likely. D:) however, we strive to provide better features. :D

# Progress

-   [x] Tokenizer (Tokens + Lexer)
-   [ ] Parser (AST) **(in progress)**
-   [ ] Compiler (Bytecode)
-   [ ] Interpreter (VM)
-   [ ] Plugin (Transcompiler)

-   [ ] Macro
-   [ ] Functional Programming
-   [ ] Type System

-   [ ] Standard Library
-   [ ] Package Manager
-   [ ] Documentation

> **Note**
>
> This plan is also subject to change during development.

---

```mermaid
graph LR
    _[Source Code] --> |sntkc| A
    A[Tokenizer / Lexer] --> B[Parser / AST]
    B --> C[ByteCode / Compiler]
    C --> D[sntk run]
    D --> |Run| E[Virtual Machine / Interpreter]
    C --> F[plugin]
    F --> G[Transcompile]
    G --> |sntkc javascript --target=ES6| H[JavaScript, etc..]
```

---

Here's what we think of language design:

```rs
use std::convert::to_s;
use std::io::println;

let foo: number[] = [1, 2, 3, 4, 5];

type T<U> = fn(U) -> U; // type alias

let bar: T<number> = fn(x) => x * 2;
let baz: T<string> = fn(x) => x.push<string>("!");

let result: string[] = foo.map(bar -> to_s -> baz);

println(result);
```

---

```rs
type Ret<T, U> = fn(T, U) -> object T: U;
let x: fn<T, U>(T, U) -> Ret<T, U> = fn<T, U>(x: T, y: U) -> fn() -> object T: U[] {
    return fn() -> object T: U[] {
        return object { "foo": x * y };
    };
};
let y: object number: number[] = x(10, 20)();
```

Parsing the above code will return following AST:

<details>
    <summary>AST</summary>
    <div markdown="1">

```
Program {
    statements: [
        TypeStatement(
            TypeStatement {
                name: Identifier {
                    value: "Ret",
                    position: Position(
                        2,
                        41,
                    ),
                },
                generics: [
                    Identifier {
                        value: "T",
                        position: Position(
                            2,
                            11,
                        ),
                    },
                    Identifier {
                        value: "U",
                        position: Position(
                            2,
                            14,
                        ),
                    },
                ],
                data_type: Fn(
                    FunctionType(
                        None,
                        [
                            Custom(
                                "T",
                            ),
                            Custom(
                                "U",
                            ),
                        ],
                        Object(
                            ObjectType(
                                Custom(
                                    "T",
                                ),
                                Custom(
                                    "U",
                                ),
                            ),
                        ),
                    ),
                ),
                position: Position(
                    2,
                    41,
                ),
            },
        ),
        LetStatement(
            LetStatement {
                name: Identifier {
                    value: "x",
                    position: Position(
                        3,
                        6,
                    ),
                },
                value: FunctionLiteral(
                    FunctionLiteral {
                        generics: Some(
                            [
                                Identifier {
                                    value: "T",
                                    position: Position(
                                        3,
                                        42,
                                    ),
                                },
                                Identifier {
                                    value: "U",
                                    position: Position(
                                        3,
                                        45,
                                    ),
                                },
                            ],
                        ),
                        parameters: [
                            (
                                Identifier {
                                    value: "x",
                                    position: Position(
                                        3,
                                        51,
                                    ),
                                },
                                Custom(
                                    "T",
                                ),
                            ),
                            (
                                Identifier {
                                    value: "y",
                                    position: Position(
                                        3,
                                        57,
                                    ),
                                },
                                Custom(
                                    "U",
                                ),
                            ),
                        ],
                        return_type: Fn(
                            FunctionType(
                                None,
                                [],
                                Object(
                                    ObjectType(
                                        Custom(
                                            "T",
                                        ),
                                        Array(
                                            Custom(
                                                "U",
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        body: BlockExpression {
                            statements: [
                                ReturnStatement(
                                    ReturnStatement {
                                        return_value: FunctionLiteral(
                                            FunctionLiteral {
                                                generics: None,
                                                parameters: [],
                                                return_type: Object(
                                                    ObjectType(
                                                        Custom(
                                                            "T",
                                                        ),
                                                        Array(
                                                            Custom(
                                                                "U",
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                                body: BlockExpression {
                                                    statements: [
                                                        ReturnStatement(
                                                            ReturnStatement {
                                                                return_value: ObjectLiteral(
                                                                    ObjectLiteral {
                                                                        pairs: [
                                                                            (
                                                                                StringLiteral(
                                                                                    StringLiteral {
                                                                                        value: "foo",
                                                                                        position: Position(
                                                                                            5,
                                                                                            29,
                                                                                        ),
                                                                                    },
                                                                                ),
                                                                                InfixExpression(
                                                                                    InfixExpression {
                                                                                        left: Identifier(
                                                                                            Identifier {
                                                                                                value: "x",
                                                                                                position: Position(
                                                                                                    5,
                                                                                                    33,
                                                                                                ),
                                                                                            },
                                                                                        ),
                                                                                        operator: Asterisk,
                                                                                        right: Identifier(
                                                                                            Identifier {
                                                                                                value: "y",
                                                                                                position: Position(
                                                                                                    5,
                                                                                                    37,
                                                                                                ),
                                                                                            },
                                                                                        ),
                                                                                        position: Position(
                                                                                            5,
                                                                                            37,
                                                                                        ),
                                                                                    },
                                                                                ),
                                                                            ),
                                                                        ],
                                                                        position: Position(
                                                                            5,
                                                                            38,
                                                                        ),
                                                                    },
                                                                ),
                                                                position: Position(
                                                                    5,
                                                                    39,
                                                                ),
                                                            },
                                                        ),
                                                    ],
                                                    position: Position(
                                                        6,
                                                        5,
                                                    ),
                                                },
                                                position: Position(
                                                    6,
                                                    5,
                                                ),
                                            },
                                        ),
                                        position: Position(
                                            6,
                                            6,
                                        ),
                                    },
                                ),
                            ],
                            position: Position(
                                7,
                                1,
                            ),
                        },
                        position: Position(
                            7,
                            1,
                        ),
                    },
                ),
                data_type: Fn(
                    FunctionType(
                        Some(
                            [
                                Identifier {
                                    value: "T",
                                    position: Position(
                                        3,
                                        12,
                                    ),
                                },
                                Identifier {
                                    value: "U",
                                    position: Position(
                                        3,
                                        15,
                                    ),
                                },
                            ],
                        ),
                        [
                            Custom(
                                "T",
                            ),
                            Custom(
                                "U",
                            ),
                        ],
                        Generic(
                            Generic(
                                Custom(
                                    "Ret",
                                ),
                                [
                                    Custom(
                                        "T",
                                    ),
                                    Custom(
                                        "U",
                                    ),
                                ],
                            ),
                        ),
                    ),
                ),
                position: Position(
                    7,
                    2,
                ),
            },
        ),
        LetStatement(
            LetStatement {
                name: Identifier {
                    value: "y",
                    position: Position(
                        8,
                        6,
                    ),
                },
                value: CallExpression(
                    CallExpression {
                        function: CallExpression(
                            CallExpression {
                                function: Identifier(
                                    Identifier {
                                        value: "x",
                                        position: Position(
                                            8,
                                            35,
                                        ),
                                    },
                                ),
                                arguments: [
                                    NumberLiteral(
                                        NumberLiteral {
                                            value: 10.0,
                                            position: Position(
                                                8,
                                                38,
                                            ),
                                        },
                                    ),
                                    NumberLiteral(
                                        NumberLiteral {
                                            value: 10.0,
                                            position: Position(
                                                8,
                                                38,
                                            ),
                                        },
                                    ),
                                    NumberLiteral(
                                        NumberLiteral {
                                            value: 20.0,
                                            position: Position(
                                                8,
                                                42,
                                            ),
                                        },
                                    ),
                                ],
                                position: Position(
                                    8,
                                    42,
                                ),
                            },
                        ),
                        arguments: [],
                        position: Position(
                            8,
                            44,
                        ),
                    },
                ),
                data_type: Object(
                    ObjectType(
                        Number,
                        Array(
                            Number,
                        ),
                    ),
                ),
                position: Position(
                    8,
                    45,
                ),
            },
        ),
    ],
    errors: [],
}
```

</details>
