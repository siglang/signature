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
    /// `Object`: a hash literal. `data: hash(key_type, value_type)`
    Object(ObjectType),
    /// a function literal. `data: fn((parameter_type)s) -> return_type`
    Fn(FunctionType),
    /// a generic type. `data: T<U>`
    Generic(Generic),
    /// a custom type. `data: T`
    Custom(String),
    /// `Void`: a void literal. `data: void`
    Void,
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
make_struct! { ReturnStatement => value: Expression }
make_struct! { ExpressionStatement => expression: Expression }
make_struct! { BlockExpression => statements: Vec<Statement> }
make_struct! { Identifier => value: String }
make_struct! { NumberLiteral => value: f64 }
make_struct! { PrefixExpression => operator: Tokens, right: Box<Expression> }
make_struct! { InfixExpression => left: Box<Expression>, operator: Tokens, right: Box<Expression> }
make_struct! { BooleanLiteral => value: bool }
make_struct! { IfExpression => condition: Box<Expression>, consequence: Box<BlockExpression>, alternative: Option<Box<BlockExpression>> }
make_struct! { FunctionLiteral => generics: Option<IdentifierGeneric>, parameters: Vec<(Identifier, DataType)>, return_type: DataType, body: BlockExpression }
make_struct! { CallExpression => function: Box<Expression>, arguments: Vec<Expression> }
make_struct! { StringLiteral => value: String }
make_struct! { ArrayLiteral => elements: Vec<Expression> }
make_struct! { IndexExpression => left: Box<Expression>, index: Box<Expression> }
make_struct! { ObjectLiteral => pairs: Vec<(StringLiteral, Expression)> }

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
