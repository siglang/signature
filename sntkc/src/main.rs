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
    println!(
        "{:#?}",
        Parser::from(
            r#"
type Ret<T, U> = fn(T, U) -> object T: U;
let x: fn<T, U>(T, U) -> Ret<T, U> = fn<T, U>(x: T, y: U) -> fn() -> object T: U[] {
    return fn() -> object T: U[] {
        return object { "foo": x * y };
    };
};
let y: object number: number[] = x(10, 20)();
        "#
        )
        .parse_program()
    );
}
