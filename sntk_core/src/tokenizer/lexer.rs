#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    char: char,
    current_position: (usize, usize),
}

