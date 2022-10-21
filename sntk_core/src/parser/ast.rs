use super::error::*;
use crate::tokenizer::token::*;

/// `Program` is the structure where the AST is finally stored.
///
/// and, this structure also stores errors that occurred during parsing.
#[derive(Debug, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<ParsingError>,
}

/// In the Sanetaka language, statements contain the following data:
///
/// * `let` statement (`let ident = value;`)
/// * `return` statement (`return value;`)
/// * `block` statement (`{ statements }`)
/// * `expression` statement (`value;`)
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    TypeStatement(TypeStatement),
    ExpressionStatement(ExpressionStatement),
}

/// `value` in Statement corresponds to Expression.
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
    /// `"Hello, World!"`: a string literal. it starts and ends with double quotes (`"`).
    StringLiteral(StringLiteral),
    /// `123.45`: a number literal. allows floating point numbers.
    NumberLiteral(NumberLiteral),
    /// `[1, 2, 3, 4]`: an array literal. it starts with `[` and ends with `]`.
    ArrayLiteral(ArrayLiteral),
    /// `true` or `false`: a boolean literal. it is a keyword.
    BooleanLiteral(BooleanLiteral),
    /// `array[index]`: an index expression. an array or hash can access it.
    IndexExpression(IndexExpression),
    /// `{ key: value, key: value }`: a hash literal. it starts with `{` and ends with `}`. a key and a value are separated by a colon (`:`).
    ObjectLiteral(ObjectLiteral),
}

/// * `Number`: a number literal. `data: number`
/// * `String`: a string literal. `data: string`
/// * `Boolean`: a boolean literal. `data: boolean`
/// * `Array`: an array literal. `data: type[]`
/// * `Hash`: a hash literal. `data: hash(key_type, value_type)`
/// * `Fn`: a function literal. `data: fn((parameter_type)s) -> return_type`
#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    Number,
    String,
    Boolean,
    Void,
    Array(Box<DataType>),
    Object(ObjectType),
    Fn(FunctionType),
    Generic(Generic),
    Custom(String),
}

/// `<T, U, V>`: a generic type. `T`, `U` and `V` are identifiers.
pub type IdentifierGeneric = Vec<Identifier>;

#[derive(Debug, PartialEq, Clone)]
pub struct ObjectType(Box<DataType>, Box<DataType>);

impl ObjectType {
    pub fn new(key_type: DataType, value_type: DataType) -> Self {
        ObjectType(Box::new(key_type), Box::new(value_type))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType(Option<IdentifierGeneric>, Vec<DataType>, Box<DataType>);

impl FunctionType {
    pub fn new(generics: Option<IdentifierGeneric>, parameters: Vec<DataType>, return_type: DataType) -> Self {
        FunctionType(generics, parameters, Box::new(return_type))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Generic(Box<DataType>, Vec<DataType>);

impl Generic {
    pub fn new(data_type: DataType, generic_types: Vec<DataType>) -> Self {
        Generic(Box::new(data_type), generic_types)
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
make_struct! { ReturnStatement => return_value: Expression }
make_struct! { ExpressionStatement => expression: Expression }
make_struct! { BlockExpression => statements: Vec<Statement> }
make_struct! { Identifier => value: String }
make_struct! { NumberLiteral => value: f64 }
make_struct! { PrefixExpression => operator: Tokens, right: Box<Expression> }
make_struct! { InfixExpression => left: Box<Expression>, operator: Tokens, right: Box<Expression> }
make_struct! { BooleanLiteral => value: bool }
make_struct! { IfExpression => condition: Box<Expression>, consequence: BlockExpression, alternative: Option<BlockExpression> }
make_struct! { FunctionLiteral => generics: Option<IdentifierGeneric>, parameters: Vec<(Identifier, DataType)>, return_type: DataType, body: BlockExpression }
make_struct! { CallExpression => function: Box<Expression>, arguments: Vec<Expression> }
make_struct! { StringLiteral => value: String }
make_struct! { ArrayLiteral => elements: Vec<Expression> }
make_struct! { IndexExpression => left: Box<Expression>, index: Box<Expression> }
make_struct! { ObjectLiteral => pairs: Vec<(Expression, Expression)> }

/// Priority is used to determine the priority of the operator.
/// The higher the priority, the higher the priority.
#[derive(Debug, PartialEq, PartialOrd)]
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
