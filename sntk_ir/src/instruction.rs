use sntk_core::{parser::ast::DataType, tokenizer::token::Tokens};
use std::fmt;

pub type Identifier = String;
pub type Block = Vec<Instruction>;

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub instruction: InstructionType,
    pub position: (usize, usize),
}

impl Instruction {
    #[inline]
    pub fn new(instruction: InstructionType, position: (usize, usize)) -> Self {
        Self { instruction, position }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "instruction({})", self.instruction)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionType {
    StoreName(Identifier, IrExpression), /* identifier, literal */
    Return(IrExpression),                /* literal */
    Expression(IrExpression),            /* expression */
}

impl fmt::Display for InstructionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StoreName(identifier, expression) => write!(f, "store_name({}, {})", identifier, expression),
            Self::Return(expression) => write!(f, "return({})", expression),
            Self::Expression(expression) => write!(f, "expression({})", expression),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrExpression {
    Identifier(Identifier),                                              /* identifier */
    Literal(LiteralValue),                                               /* literal */
    Block(Block),                                                        /* block */
    If(Box<IrExpression>, Box<IrExpression>, Box<Option<IrExpression>>), /* condition, consequence, alternative */
    Call(Box<IrExpression>, Vec<IrExpression>),                          /* function, arguments */
    Index(Box<IrExpression>, Box<IrExpression>),                         /* left, index */
    Prefix(Tokens, Box<IrExpression>),                                   /* operator, right */
    Infix(Box<IrExpression>, Tokens, Box<IrExpression>),                 /* left, operator, right */
}

impl fmt::Display for IrExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "expression")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Number(f64),                                            /* number */
    String(String),                                         /* string */
    Boolean(bool),                                          /* boolean */
    Array(Vec<IrExpression>),                               /* array */
    Function(Vec<(Identifier, DataType)>, Block, DataType), /* (parameters, datatype), block, return type */
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralValue::Number(number) => write!(f, "{}", number),
            LiteralValue::String(string) => write!(f, "{}", string),
            LiteralValue::Boolean(boolean) => write!(f, "{}", boolean),
            LiteralValue::Array(array) => write!(f, "[{}]", array.iter().map(|element| format!("{}", element)).collect::<String>()),
            LiteralValue::Function(parameters, _, data_type) => {
                write!(
                    f,
                    "fn({}) -> {}",
                    parameters
                        .iter()
                        .map(|parameter| format!("{}: {}", parameter.0, parameter.1))
                        .collect::<String>(),
                    data_type
                )
            }
        }
    }
}
