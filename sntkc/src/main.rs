use sntk_core::{options::*, parser::parser::*, tokenizer::lexer::*};

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

    println!(
        "{:#?}",
        Parser {
            lexer: Lexer::new(r#"3 == (-1 + 4);"#),
            options: CompilerOptions {
                eee_opt_level: 2.into(),
                ..Default::default()
            },
            ..Default::default()
        }
        .parse_program()
    );
}
