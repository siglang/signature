use sntk_bytecode::{code::*, interpreter::*, stack::*};
// use sntk_core::{options::*, parser::parser::*, tokenizer::lexer::*};

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
    // b = a * 10; // b = 70
    // print(b + a); // prints 77
    Interpreter::new(
        vec![
            /* Line 1 */ Instruction::LoadConst(0), // 5
            /* Line 1 */ Instruction::LoadConst(1), // 2
            /* Line 1 */ Instruction::BinaryOp(BinaryOp::Add), // 5 + 2
            /* Line 1 */ Instruction::StoreName(0), // a = 7
            /* Line 2 */ Instruction::LoadName(0), // a, 7
            /* Line 2 */ Instruction::LoadGlobal(0), // print
            /* Line 2 */ Instruction::CallFunction(1), // print(7)
            /* Line 3 */ Instruction::LoadName(0), // a, 7
            /* Line 3 */ Instruction::LoadConst(2), // 10
            /* Line 3 */ Instruction::BinaryOp(BinaryOp::Mul), // 7 * 10
            /* Line 3 */ Instruction::StoreName(1), // b = 70
            /* Line 4 */ Instruction::LoadName(1), // b, 70
            /* Line 4 */ Instruction::LoadName(0), // a, 7
            /* Line 4 */ Instruction::BinaryOp(BinaryOp::Add), // 70 + 7
            /* Line 4 */ Instruction::LoadGlobal(0), // print
            /* Line 4 */ Instruction::CallFunction(1), // print(77)
        ],
        vec![
            Value::LiteralValue(LiteralValue::Number(5.0)),
            Value::LiteralValue(LiteralValue::Number(2.0)),
            Value::LiteralValue(LiteralValue::Number(10.0)),
        ],
        vec!["print".to_string(), "a".to_string(), "b".to_string()],
    )
    .run();
}
