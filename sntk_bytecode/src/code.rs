use crate::stack::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    LoadGlobal(String),
    LoadConst(Value),
    CallFunction(usize),
    StoreName(String),
    LoadName(String),
    JumpIfTrue(usize),
    JumpIfFalse(usize),
    Jump(usize),
    Return,
    BinaryOp(BinaryOp),
    BinaryOpEq(BinaryOpEq),
    UnaryOp(UnaryOp),
    Block(Vec<Instruction>),
}

macro_rules! binary_op {
    ($( $op:ident )*) => {
        /// **Binary operations**
        #[derive(Debug, PartialEq, Clone)]
        pub enum BinaryOp {
            $( $op ),*
        }
    }
}

macro_rules! binary_op_eq {
    ($( $op:ident )*) => {
        /// **Binary operations**
        #[derive(Debug, PartialEq, Clone)]
        pub enum BinaryOpEq {
            $( $op ),*
        }
    }
}

macro_rules! unary_op {
    ($( $op:ident )*) => {
        /// **Unary operations**
        #[derive(Debug, PartialEq, Clone)]
        pub enum UnaryOp {
            $( $op ),*
        }
    }
}

binary_op! { Add Sub Mul Div Mod }
binary_op_eq! { Eq Neq Lt Gt Lte Gte }
unary_op! { Not Minus }
