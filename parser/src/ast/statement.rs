use super::{DataType, Expression, Identifier, IdentifierGeneric, Position};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ReturnExpressionStatement(ReturnExpressionStatement),
    TypeStatement(TypeStatement),
    DeclareStatement(DeclareStatement),
    StructStatement(StructStatement),
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub identifier: Identifier,
    pub value: Expression,
    pub data_type: DataType,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeStatement {
    pub identifier: Identifier,
    pub data_type: DataType,
    pub generics: IdentifierGeneric,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DeclareStatement {
    pub identifier: Identifier,
    pub data_type: DataType,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructStatement {
    pub identifier: Identifier,
    pub generics: IdentifierGeneric,
    pub fields: Vec<(Identifier, DataType)>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub value: Expression,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnExpressionStatement {
    pub value: Expression,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
    pub position: Position,
}
