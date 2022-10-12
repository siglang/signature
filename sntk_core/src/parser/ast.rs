/// `Program` is the structure where the AST is finally stored.
///
/// and, this structure also stores errors that occurred during parsing.
#[derive(Debug, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<String>,
}

/// In the Sanetaka language, statements contain the following data:
///
/// * `let` statement (`let ident = value;`)
/// * `return` statement (`return value;`)
/// * `block` statement (`{ statements }`)
/// * `expression` statement (`value;`)
#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    BlockStatement(BlockStatement),
    ExpressionStatement(ExpressionStatement),
}

/// `value` in Statement corresponds to Expression.
///
/// when expression ends, a semicolon (`;`) is required.
#[derive(Debug, PartialEq)]
pub enum Expression {
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
    Boolean(Boolean),
    /// `array[index]`: an index expression. an array or hash can access it.
    IndexExpression(IndexExpression),
    /// `{ key: value, key: value }`: a hash literal. it starts with `{` and ends with `}`. a key and a value are separated by a colon (`:`).
    HashLiteral(HashLiteral),
}

/// * `Number`: a number literal. `data: number`
/// * `String`: a string literal. `data: string`
/// * `Boolean`: a boolean literal. `data: boolean`
/// * `Array`: an array literal. `data: array[]`
/// * `Hash`: a hash literal. `data: hash(key_type, value_type)`
/// * `Fn`: a function literal. `data: fn((parameter_type)s) -> return_type`
#[derive(Debug, PartialEq)]
pub enum DataType {
    Number,
    String,
    Boolean,
    Array(Box<DataType>),
    Hash(Box<DataType>, Box<DataType>),
    Fn(Vec<DataType>, Box<DataType>),
}

/// The Position structure is to indicate the exact position of the error message.
/// there is nothing else to do.
#[derive(Debug, PartialEq, Default)]
pub struct Position(pub usize, pub usize);

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Position(line, column)
    }
}

macro_rules! make_struct {
    ($name:ident => $( $field:ident: $type:ty ),*) => {
        #[derive(Debug, PartialEq)]
        pub struct $name {
            $( pub $field: $type, )*
            position: Position
        }

        impl $name {
            pub fn new($( $field: $type, )*) -> Self {
                $name { $($field,)* position: Position::default() }
            }
        }
    };
    (@data_type $name:ident => $( $field:ident: $type:ty ),*) => {
        #[derive(Debug, PartialEq)]
        pub struct $name {
            $( pub $field: $type, )*
            pub data_type: DataType,
            position: Position
        }

        impl $name {
            pub fn new(data_type: DataType, $( $field: $type, )*) -> Self {
                $name { $($field,)* data_type, position: Position::default() }
            }
        }
    }
}

make_struct! { @data_type LetStatement => name: Identifier, value: Expression }
make_struct! { @data_type ReturnStatement => return_value: Expression }
make_struct! { ExpressionStatement => expression: Expression }
make_struct! { BlockStatement => statements: Vec<Statement> }
make_struct! { Identifier => value: String }
make_struct! { NumberLiteral => value: i64 }
make_struct! { PrefixExpression => operator: String, right: Box<Expression> }
make_struct! { InfixExpression => left: Box<Expression>, operator: String, right: Box<Expression> }
make_struct! { Boolean => value: bool }
make_struct! { IfExpression => condition: Box<Expression>, consequence: BlockStatement, alternative: Option<BlockStatement> }
make_struct! { FunctionLiteral => parameters: Vec<Identifier>, body: BlockStatement }
make_struct! { CallExpression => function: Box<Expression>, arguments: Vec<Expression> }
make_struct! { StringLiteral => value: String }
make_struct! { ArrayLiteral => elements: Vec<Expression> }
make_struct! { IndexExpression => left: Box<Expression>, index: Box<Expression> }
make_struct! { HashLiteral => pairs: Vec<(Expression, Expression)> }
