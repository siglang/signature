use super::{BlockExpression, DataType, Expression, IdentifierGeneric, Parameter, Position};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Identifier(Identifier),
    NumberLiteral(NumberLiteral),
    StringLiteral(StringLiteral),
    BooleanLiteral(BooleanLiteral),
    FunctionLiteral(FunctionLiteral),
    ArrayLiteral(ArrayLiteral),
    StructLiteral(StructLiteral),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumberLiteral {
    pub value: f64,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringLiteral {
    pub value: String,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BooleanLiteral {
    pub value: bool,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
    pub parameters: Vec<Parameter>,
    pub body: BlockExpression,
    pub generics: Option<IdentifierGeneric>,
    pub return_type: DataType,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructLiteral {
    pub identifier: Identifier,
    pub fields: Vec<(Identifier, Expression)>,
    pub position: Position,
}
