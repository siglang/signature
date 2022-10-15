use sntk_core::parser::parser::*;

fn main() {
    println!(
        "{:#?}",
        Parser::from(r#"let x: T<U[], V, L> = 10; return (3); 2;"#).parse_program()
    );

    println!(
        "{:#?}",
        Parser::from(r#"type x<T, U> = T<>;"#).parse_program()
    );

    println!(
        "{:#?}",
        Parser::from(r#"type func<T, U> = fn(T, U[]) -> string;"#).parse_program()
    );

    println!("{:#?}", Parser::from(r#"1 * 2;"#).parse_program());
}
