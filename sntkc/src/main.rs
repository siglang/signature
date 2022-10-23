#![allow(unused_imports)]

use sntk_bytecode::{code::*, interpreter::*, stack::*};
use sntk_core::{options::*, parser::parser::*, tokenizer::lexer::*};
use sntk_compiler::compiler::*;
use sntk_core::parser::parser::*;

fn main() {
    // println!(
    //     "{:#?}",
    //     Parser::from(r#"let x: T<U[], V, L> = 10; return (3); 2;"#).parse_program()
    // );
    // println!(
    //     "{:#?}",
    //     Parser::from(r#"type X<T, U> = T<>;"#).parse_program()
    // );
    // println!(
    //     "{:#?}",
    //     Parser::from(r#"type X<T, U> = fn(T, U[]) -> object T: U;"#).parse_program()
    // );
    // println!("{:#?}", Parser::from(r#"[1, 2, 3];"#).parse_program());
    // println!(
    //     "{:#?}",
    //     Parser::from(r#"object {"x": 10, 2: 3};"#).parse_program()
    // );
    // println!(
    //     "{:#?}",
    //     Parser::from(r#"{{{{{{{{{};};};};};};};};};"#).parse_program()
    // );
    //     println!(
    //         "{:#?}",
    //         Parser::from(
    //             r#"
    // type Ret<T, U> = fn(T, U) -> object T: U;
    // let x: fn<T, U>(T, U) -> Ret<T, U> = fn<T, U>(x: T, y: U) -> fn() -> object T: U[] {
    //     println("Hello, World!");
    //     return fn() -> object T: U[] -> object { "foo": x * y };
    // };
    // let y: object number: number[] = x(10, 20)();
    //             "#
    //         )
    //         .parse_program()
    //     );

    // println!(
    //     "{:#?}",
    //     Parser {
    //         lexer: Lexer::new(r#"3 == (-1 + 4);"#),
    //         options: CompilerOptions {
    //             eee_opt_level: 2.into(),
    //             ..Default::default()
    //         },
    //         ..Default::default()
    //     }
    //     .parse_program()
    // );

    // a = 5 + 2; // a = 7
    // print(a); // prints 7
    // foo(a, 10, 20, "hello" + "world");
    // a = (a + 1) * 2; // a = 16
    // print(a); // prints 16
    // Interpreter::new(
    //     vec![
    //         /* Line 1 */ Instruction::LoadConst(0), // 5
    //         /* Line 1 */ Instruction::LoadConst(1), // 2
    //         /* Line 1 */ Instruction::BinaryOp(BinaryOp::Add), // 5 + 2
    //         /* Line 1 */ Instruction::StoreName(0), // a = 7
    //         /* Line 2 */ Instruction::LoadName(0), // a, 7
    //         /* Line 2 */ Instruction::LoadGlobal(1), // print
    //         /* Line 2 */ Instruction::CallFunction(1), // print(7)
    //         /* Line 3 */ Instruction::LoadName(0), // a, 7
    //         /* Line 3 */ Instruction::LoadConst(2), // 10
    //         /* Line 3 */ Instruction::LoadConst(3), // 20
    //         /* Line 3 */ Instruction::LoadConst(4), // "hello"
    //         /* Line 3 */ Instruction::LoadConst(5), // "world"
    //         /* Line 3 */ Instruction::BinaryOp(BinaryOp::Add), // "hello" + "world"
    //         /* Line 3 */ Instruction::LoadGlobal(2), // foo
    //         /* Line 3 */ Instruction::CallFunction(4), // foo(7, 10, 20, "helloworld")
    //         /* Line 4 */ Instruction::LoadName(0), // a, 7
    //         /* Line 4 */ Instruction::LoadConst(1), // 2
    //         /* Line 4 */ Instruction::BinaryOp(BinaryOp::Add), // 7 + 2
    //         /* Line 4 */ Instruction::LoadConst(6), // 2
    //         /* Line 4 */ Instruction::BinaryOp(BinaryOp::Mul), // (7 + 2) * 2
    //         /* Line 4 */ Instruction::StoreName(0), // a = 16
    //         /* Line 5 */ Instruction::LoadName(0), // a, 16
    //         /* Line 5 */ Instruction::LoadGlobal(1), // print
    //         /* Line 5 */ Instruction::CallFunction(1), // print(16)
    //     ],
    //     vec![
    //         Value::LiteralValue(LiteralValue::Number(5.0)),
    //         Value::LiteralValue(LiteralValue::Number(2.0)),
    //         Value::LiteralValue(LiteralValue::Number(10.0)),
    //         Value::LiteralValue(LiteralValue::Number(20.0)),
    //         Value::LiteralValue(LiteralValue::String("hello".to_string())),
    //         Value::LiteralValue(LiteralValue::String("world".to_string())),
    //         Value::LiteralValue(LiteralValue::Number(2.0)),
    //     ],
    //     vec![
    //         "a".to_string(),
    //         "print".to_string(),
    //         "foo".to_string(),
    //     ],
    // ).run();

    // if (true) {
    //   print("Hello, World!");
    // } else {
    //   print("Goodbye, World!");
    // }
    // print("Foo");
    // Interpreter::new(
    //     vec![
    //         /* 0  */ /* Line 1 */ Instruction::LoadConst(0), // true
    //         /* 1  */ /* Line 1 */ Instruction::JumpIfFalse(5), // if (true) {
    //         /* 2  */ /* Line 2 */ Instruction::LoadConst(1), // "Hello, World!"
    //         /* 3  */ /* Line 2 */ Instruction::LoadGlobal(0), // print
    //         /* 4  */ /* Line 2 */ Instruction::CallFunction(1), // print("Hello, World!")
    //         /* 5  */ /* Line 3 */ Instruction::Jump(8), // } else {
    //         /* 6  */ /* Line 4 */ Instruction::LoadConst(2), // "Goodbye, World!"
    //         /* 7  */ /* Line 4 */ Instruction::LoadGlobal(0), // print
    //         /* 8  */ /* Line 4 */ Instruction::CallFunction(1), // print("Goodbye, World!")
    //         /* 9  */ /* Line 6 */ Instruction::LoadConst(3), // "Foo"
    //         /* 10 */ /* Line 6 */ Instruction::LoadGlobal(0), // print
    //         /* 11 */ /* Line 6 */ Instruction::CallFunction(1), // print("Foo")
    //     ],
    //     vec![
    //         Value::LiteralValue(LiteralValue::Boolean(true)),
    //         Value::LiteralValue(LiteralValue::String("Hello, World!".to_string())),
    //         Value::LiteralValue(LiteralValue::String("Goodbye, World!".to_string())),
    //         Value::LiteralValue(LiteralValue::String("Foo".to_string())),
    //     ],
    //     vec!["print".to_string()],
    // )
    // .run();

    // // let a = -1;
    // // print(a > 2);
    // Interpreter::new(
    //     vec![
    //         /* 0 */ /* Line 1 */ Instruction::LoadConst(0), // -1
    //         /* 1 */ /* Line 1 */ Instruction::StoreName(0), // a = -1
    //         /* 2 */ /* Line 2 */ Instruction::LoadName(0), // a, -1
    //         /* 3 */ /* Line 2 */ Instruction::LoadConst(1), // 2
    //         /* 4 */ /* Line 2 */ Instruction::BinaryOpEq(BinaryOpEq::Gt), // -1 > 2
    //         /* 5 */ /* Line 2 */ Instruction::LoadGlobal(0), // print
    //         /* 6 */ /* Line 2 */ Instruction::CallFunction(1), // print(-1 > 2)
    //     ],
    //     vec![
    //         Value::LiteralValue(LiteralValue::Number(-1.0)),
    //         Value::LiteralValue(LiteralValue::Number(2.0)),
    //     ],
    //     vec!["print".to_string()],
    // )
    // .run();

    let source = r#"
let x: number = 10;
let y: number = 20.3;
let z: number = 3;
    "#;

    let mut c = Compiler::new(Parser::from(source).parse_program()).compile_program().unwrap();
    c.run();

    println!("{:#?}", c);

    // source compiled to bytecode
    Interpreter::new(
        vec![
            /* 0 */ /* Line 1 */ Instruction::LoadConst(0), // 10
            /* 1 */ /* Line 1 */ Instruction::StoreName(0), // x = 10
            /* 2 */ /* Line 2 */ Instruction::LoadConst(1), // 20.3
            /* 3 */ /* Line 2 */ Instruction::StoreName(1), // y = 20.3
            /* 4 */ /* Line 3 */ Instruction::LoadConst(2), // 3
            /* 5 */ /* Line 3 */ Instruction::StoreName(2), // z = 3
        ],
        vec![
            Value::LiteralValue(LiteralValue::Number(10.0)),
            Value::LiteralValue(LiteralValue::Number(20.3)),
            Value::LiteralValue(LiteralValue::Number(3.0)),
        ],
        vec![
            "x".to_string(),
            "y".to_string(),
            "z".to_string(),
        ],
    )
    .run();
}
