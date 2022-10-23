macro_rules! instruction {
    ($( $op:ident )*) => {
        /// **Instruction commands**
        #[derive(Debug, PartialEq, Clone)]
        pub enum Instruction {
            $( $op(usize) ),*,
            BinaryOp(BinaryOp),
            BinaryOpEq(BinaryOpEq),
            UnaryOp(UnaryOp),
        }
    };
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

instruction! { LoadGlobal LoadConst CallFunction StoreName LoadName DeleteName JumpIfTrue JumpIfFalse Jump }
binary_op! { Add Sub Mul Div Mod }
binary_op_eq! { Eq Neq Lt Gt Lte Gte }
unary_op! { Not Minus }
