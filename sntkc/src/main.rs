use sntk_core::parser::parser::*;

fn main() {
    println!(
        "{:#?}",
        Parser::from(r#"let x: T<U[], V, L> = 10; return (3); 2;"#).parse_program()
    );
    println!(
        "{:#?}",
        Parser::from(r#"type x<T, U> = T<U[]>;"#).parse_program()
    );
}
