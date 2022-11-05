use sntk_core::parser::ast::Position;

#[inline(always)]
pub fn ast_position_to_tuple(position: &Position) -> (usize, usize) {
    (position.0, position.1)
}
