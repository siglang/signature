use crate::stack::Value;

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    LoadGlobal(String),
    LoadConst(Value),
    CallFunction(usize),
    StoreName(String),
    LoadName(String),
    If(Block, Option<Block>),
    BinaryOp(BinaryOp),
    BinaryOpEq(BinaryOpEq),
    UnaryOp(UnaryOp),
    Block(Block),
    Return,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block(pub Vec<Instruction>);

macro_rules! binary_op {
    ($( $op:ident )*) => {
        #[derive(Debug, Eq, PartialEq, Clone)]
        pub enum BinaryOp { $( $op ),* }
    }
}

macro_rules! binary_op_eq {
    ($( $op:ident )*) => {
        #[derive(Debug, Eq, PartialEq, Clone)]
        pub enum BinaryOpEq { $( $op ),* }
    }
}

macro_rules! unary_op {
    ($( $op:ident )*) => {
        #[derive(Debug, Eq, PartialEq, Clone)]
        pub enum UnaryOp { $( $op ),* }
    }
}

binary_op! { Add Sub Mul Div Mod }
binary_op_eq! { Eq Neq Lt Gt Lte Gte }
unary_op! { Not Minus }
