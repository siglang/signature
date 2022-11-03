use crate::{parser::error::ParsingError, tokenizer::token::Tokens};

/// **`Program` is the structure where the AST is finally stored.**
///
/// and, this structure also stores errors that occurred during parsing.
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

/// **In the Sanetaka language, statements contain the following data:**
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    /// `let` statement (`let ident = value;`)
    LetStatement(LetStatement),
    /// `return` statement (`return value;`)
    ReturnStatement(ReturnStatement),
    /// `type` statement (`type Ident = T;`)
    TypeStatement(TypeStatement),
    /// `struct` statement (`struct Ident { field: T, ... };`)
    StructStatement(StructStatement),
    /// expression statement (`value;`)
    ExpressionStatement(ExpressionStatement),
}

/// **`value` in Statement corresponds to Expression.**
///
/// when expression ends, a semicolon (`;`) is required.
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BlockExpression(BlockExpression),
    /// `ident`: it is an identifier, not a keyword, such as a variable name.
    Identifier(Identifier),
    /// `!value`: a prefix before an expression. this includes `!`, `-` and etc.
    PrefixExpression(PrefixExpression),
    /// `value + value`: a infix between two expressions. this includes `+`, `-`, `*` and etc.
    InfixExpression(InfixExpression),
    /// `if (condition) { consequence } else { alternative }`: a conditional expression. `else` can be omitted.
    IfExpression(IfExpression),
    /// `fn (parameters) { body }`: a function expression. `parameters` is a list of identifiers.
    FunctionLiteral(FunctionLiteral),
    /// `function(arguments)`: a function call expression. `arguments` is a list of expressions.
    CallExpression(CallExpression),
    /// `typeof value`: a type expression. this expression returns the type of the value.
    TypeofExpression(TypeofExpression),
    /// `"Hello, World!"`: a string literal. it starts and ends with double quotes (`"`).
    StringLiteral(StringLiteral),
    /// `123.45`: a number literal. allows floating point numbers.
    NumberLiteral(NumberLiteral),
    /// `[1, 2, 3, 4]`: an array literal. it starts with `[` and ends with `]`.
    ArrayLiteral(ArrayLiteral),
    /// `true` or `false`: a boolean literal. it is a keyword.
    BooleanLiteral(BooleanLiteral),
    /// `array[index]`: an index expression. an array can access it.
    IndexExpression(IndexExpression),
    /// `struct { field: value, ... }`: a struct literal.` 
    StructLiteral(StructLiteral),
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    /// a number literal. `data: number`
    Number,
    /// a string literal. `data: string`
    String,
    /// a boolean literal. `data: boolean`
    Boolean,
    /// an array literal. `data: type[]`
    Array(Box<DataType>),
    /// a function literal. `data: fn((parameter_type)s) -> return_type`
    Fn(FunctionType),
    /// a generic type. `data: T<U>`
    Generic(Generic),
    /// a custom type. `data: T`
    Custom(String),
    /// `Void`: a void literal. `data: void`
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
            DataType::Void => write!(f, "void"),
            DataType::Unknown => write!(f, "Unknown"),
        }
    }
}

/// `<T, U, V>`: a generic type. `T`, `U` and `V` are identifiers.
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

/// The Position structure is to indicate the exact position of the error message.
/// there is nothing else to do.
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

/// Priority is used to determine the priority of the operator.
/// The higher the priority, the higher the priority.
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
