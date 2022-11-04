use crate::{parser::error::ParsingError, tokenizer::token::Tokens};

#[derive(Debug, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<ParsingError>,
}

impl Program {
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
    ReturnStatement(ReturnStatement),
    TypeStatement(TypeStatement),
    StructStatement(StructStatement),
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BlockExpression(BlockExpression),
    Identifier(Identifier),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    TypeofExpression(TypeofExpression),
    StringLiteral(StringLiteral),
    NumberLiteral(NumberLiteral),
    ArrayLiteral(ArrayLiteral),
    BooleanLiteral(BooleanLiteral),
    IndexExpression(IndexExpression),
    StructLiteral(StructLiteral),
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    Number,
    String,
    Boolean,
    Array(Box<DataType>),
    Fn(FunctionType),
    Generic(Generic),
    Custom(String),
    Void,
    Unknown,
}

impl std::fmt::Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            DataType::Number => write!(f, "Number"),
            DataType::String => write!(f, "String"),
            DataType::Boolean => write!(f, "Boolean"),
            DataType::Array(data_type) => write!(f, "{}[]", data_type),
            DataType::Fn(function_type) => write!(f, "{}", function_type),
            DataType::Generic(generic) => write!(f, "{}", generic),
            DataType::Custom(name) => write!(f, "{}", name),
            DataType::Void => write!(f, "Void"),
            DataType::Unknown => write!(f, "Unknown"),
        }
    }
}

pub type IdentifierGeneric = Vec<Identifier>;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType(pub Option<IdentifierGeneric>, pub Vec<DataType>, pub Box<DataType>);

impl FunctionType {
    pub fn new(generics: Option<IdentifierGeneric>, parameters: Vec<DataType>, return_type: DataType) -> Self {
        FunctionType(generics, parameters, Box::new(return_type))
    }
}

impl std::fmt::Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        // TODO: generics
        let parameters = self.1.iter().map(|parameter| parameter.to_string()).collect::<Vec<String>>().join(", ");
        write!(f, "fn({}) -> {}", parameters, self.2)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Generic(pub Box<DataType>, pub Vec<DataType>);

impl Generic {
    pub fn new(data_type: DataType, generic_types: Vec<DataType>) -> Self {
        Generic(Box::new(data_type), generic_types)
    }
}

impl std::fmt::Display for Generic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let generic_types = self
            .1
            .iter()
            .map(|generic_type| generic_type.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{}<{}>", self.0, generic_types)
    }
}

#[derive(Debug, Default, Clone)]
pub struct Position(pub usize, pub usize);

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Position(line, column)
    }
}

impl PartialEq for Position {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

macro_rules! make_struct {
    ($name:ident => $( $field:ident: $type:ty ),*) => {
        #[derive(Debug, PartialEq, Clone)]
        pub struct $name {
            $( pub $field: $type, )*
            pub position: Position
        }

        impl $name {
            pub fn new($( $field: $type, )* position: Position) -> Self {
                $name { $($field,)* position }
            }
        }
    };
    (@data_type $name:ident => $( $field:ident: $type:ty ),*) => {
        #[derive(Debug, PartialEq, Clone)]
        pub struct $name {
            $( pub $field: $type, )*
            pub data_type: DataType,
            pub position: Position
        }

        impl $name {
            pub fn new(data_type: DataType, $( $field: $type, )* position: Position) -> Self {
                $name { $($field,)* data_type, position }
            }
        }
    }
}

make_struct! { @data_type LetStatement => name: Identifier, value: Expression }
make_struct! { @data_type TypeStatement => name: Identifier, generics: IdentifierGeneric }

make_struct! { ReturnStatement => value: Expression }
make_struct! { StructStatement => name: Identifier, generics: IdentifierGeneric, fields: Vec<(Identifier, DataType)> }
make_struct! { ExpressionStatement => expression: Expression }

make_struct! { BlockExpression => statements: Vec<Statement> }
make_struct! { IfExpression => condition: Box<Expression>, consequence: Box<BlockExpression>, alternative: Option<Box<BlockExpression>> }
make_struct! { CallExpression => function: Box<Expression>, arguments: Vec<Expression> }
make_struct! { TypeofExpression => expression: Box<Expression> }
make_struct! { IndexExpression => left: Box<Expression>, index: Box<Expression> }
make_struct! { PrefixExpression => operator: Tokens, right: Box<Expression> }
make_struct! { InfixExpression => left: Box<Expression>, operator: Tokens, right: Box<Expression> }

make_struct! { Identifier => value: String }
make_struct! { NumberLiteral => value: f64 }
make_struct! { StringLiteral => value: String }
make_struct! { BooleanLiteral => value: bool }
make_struct! { FunctionLiteral => generics: Option<IdentifierGeneric>, parameters: Vec<(Identifier, DataType)>, return_type: DataType, body: BlockExpression }
make_struct! { ArrayLiteral => elements: Vec<Expression> }
make_struct! { StructLiteral => name: Identifier, fields: Vec<(Identifier, Expression)> }

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
