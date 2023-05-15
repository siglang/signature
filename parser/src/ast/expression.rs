use super::{Literal, Position, Statement};
use crate::tokenizer::TokenKind;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BlockExpression(BlockExpression),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    CallExpression(CallExpression),
    TypeofExpression(TypeofExpression),
    IndexExpression(IndexExpression),
    Literal(Literal),
    Debug(Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockExpression {
    pub statements: Vec<Statement>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: Box<BlockExpression>,
    pub alternative: Option<Box<BlockExpression>>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeofExpression {
    pub expression: Box<Expression>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: TokenKind,
    pub right: Box<Expression>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: TokenKind,
    pub right: Box<Expression>,
    pub position: Position,
}
