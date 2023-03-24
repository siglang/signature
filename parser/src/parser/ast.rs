use crate::{parser::ParsingError, tokenizer::TokenKind};
use std::fmt;

#[derive(Debug, Default, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<ParsingError>,
}

impl Program {
    #[inline]
    pub fn new(statements: Vec<Statement>) -> Self {
        Self {
            statements,
            errors: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    AutoStatement(AutoStatement),
    ReturnStatement(ReturnStatement),
    TypeStatement(TypeStatement),
    DeclareStatement(DeclareStatement),
    StructStatement(StructStatement),
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    BlockExpression(BlockExpression),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    TypeofExpression(TypeofExpression),
    IndexExpression(IndexExpression),
    StringLiteral(StringLiteral),
    NumberLiteral(NumberLiteral),
    ArrayLiteral(ArrayLiteral),
    BooleanLiteral(BooleanLiteral),
    StructLiteral(StructLiteral),
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.data_type)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataTypeKind {
    Number,
    String,
    Boolean,
    Array(Box<DataType>),
    Fn(FunctionType),
    Generic(Generic),
    Custom(String),
    Auto,
    Unknown,
}

impl fmt::Display for DataTypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            DataTypeKind::Number | DataTypeKind::String | DataTypeKind::Boolean => {
                write!(f, "{self:?}")
            }
            DataTypeKind::Array(data_type) => write!(f, "{data_type}[]"),
            DataTypeKind::Fn(function_type) => write!(f, "{function_type}"),
            DataTypeKind::Generic(generic) => write!(f, "{generic}"),
            DataTypeKind::Custom(identifier) => write!(f, "{identifier}"),
            DataTypeKind::Auto => write!(f, "Auto"),
            DataTypeKind::Unknown => write!(f, "Unknown"),
        }
    }
}

pub type IdentifierGeneric = Vec<Identifier>;

impl std::fmt::Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let parameters = self
            .parameters
            .iter()
            .map(|parameter| parameter.0.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        let generics = match &self.generics {
            Some(generics) => {
                let generics = generics
                    .iter()
                    .map(|generic| generic.value.clone())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("<{generics}>")
            }
            None => String::new(),
        };
        write!(f, "fn{generics}({parameters}) -> {}", self.return_type)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Generic(pub Box<DataType>, pub Vec<DataType>);

impl Generic {
    #[inline]
    pub fn new(data_type: DataType, generic_types: Vec<DataType>) -> Self {
        Generic(Box::new(data_type), generic_types)
    }
}

impl std::fmt::Display for Generic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let generic_types = self
            .1
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{}<{generic_types}>", self.0)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Position(pub usize, pub usize);

impl Position {
    #[inline]
    pub fn new(line: usize, column: usize) -> Self {
        Position(line, column)
    }
}

impl PartialEq for Position {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "[{}:{}]", self.0, self.1)
    }
}

macro_rules! make_struct {
    ($identifier:ident => $( $field:ident: $type:ty ),*) => {
        #[derive(Debug, PartialEq, Clone)]
        pub struct $identifier {
            $( pub $field: $type, )*
            pub position: Position
        }

        impl $identifier {
            #[inline]
            pub fn new($( $field: $type, )* position: Position) -> Self {
                $identifier { $($field,)* position: position.clone() }
            }
        }
    };
    (@data_type $identifier:ident => $( $field:ident: $type:ty ),*) => {
        #[derive(Debug, PartialEq, Clone)]
        pub struct $identifier {
            $( pub $field: $type, )*
            pub data_type: DataType,
            pub position: Position
        }

        impl $identifier {
            #[inline]
            pub fn new(data_type: DataType, $( $field: $type, )* position: Position) -> Self {
                $identifier { $($field,)* data_type, position: position.clone() }
            }
        }
    }
}

make_struct! { DataType => data_type: DataTypeKind }

make_struct! { @data_type LetStatement => identifier: Identifier, value: Expression }
make_struct! { @data_type TypeStatement => identifier: Identifier, generics: IdentifierGeneric }
make_struct! { @data_type DeclareStatement => identifier: Identifier }

make_struct! { AutoStatement => identifier: Identifier, value: Expression }
make_struct! { StructStatement => identifier: Identifier, generics: IdentifierGeneric, fields: Vec<(Identifier, DataType)> }
make_struct! { ReturnStatement => value: Expression }
make_struct! { ExpressionStatement => expression: Expression }

make_struct! { BlockExpression => statements: Vec<Statement> }
make_struct! { IfExpression => condition: Box<Expression>, consequence: Box<BlockExpression>, alternative: Option<Box<BlockExpression>> }
make_struct! { CallExpression => function: Box<Expression>, arguments: Vec<Expression> }
make_struct! { TypeofExpression => expression: Box<Expression> }
make_struct! { IndexExpression => left: Box<Expression>, index: Box<Expression> }
make_struct! { PrefixExpression => operator: TokenKind, right: Box<Expression> }
make_struct! { InfixExpression => left: Box<Expression>, operator: TokenKind, right: Box<Expression> }

make_struct! { Identifier => value: String }
make_struct! { NumberLiteral => value: f64 }
make_struct! { StringLiteral => value: String }
make_struct! { BooleanLiteral => value: bool }
make_struct! { FunctionLiteral => generics: Option<IdentifierGeneric>, parameters: Vec<Parameter>, return_type: DataType, body: BlockExpression }
make_struct! { ArrayLiteral => elements: Vec<Expression> }
make_struct! { StructLiteral => identifier: Identifier, fields: Vec<(Identifier, Expression)> }

make_struct! { Parameter => identifier: Identifier, data_type: DataType, kind: ParameterKind }
make_struct! { FunctionType => generics: Option<IdentifierGeneric>, parameters: Vec<(DataType, ParameterKind)>, return_type: Box<DataType> }

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ParameterKind {
    Normal,
    Spread,
}

#[derive(Debug, Eq, PartialEq, PartialOrd)]
pub enum Priority {
    Lowest,
    Dot,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}
