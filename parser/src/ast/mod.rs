pub mod expression;
pub mod literal;
pub mod statement;

pub use expression::*;
pub use literal::*;
pub use statement::*;
use std::fmt;

pub type Program = Vec<Statement>;

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.kind)
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
#[derive(Debug, PartialEq, Clone)]
pub struct DataType {
    pub kind: DataTypeKind,
    pub position: Position,
}

impl DataType {
    pub fn new(data_type: DataTypeKind, position: Position) -> Self {
        Self {
            kind: data_type,
            position,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parameter {
    pub identifier: Identifier,
    pub data_type: DataType,
    pub kind: ParameterKind,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType {
    pub generics: Option<IdentifierGeneric>,
    pub parameters: Vec<(DataType, ParameterKind)>,
    pub return_type: Box<DataType>,
    pub position: Position,
}

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
