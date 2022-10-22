macro_rules! instruction {
    ($( $op:ident )*) => {
        /// **Instruction commands**
        #[derive(Debug, PartialEq, Clone)]
        pub enum Instruction {
            $( $op(usize) ),*,
            BinaryOp(BinaryOp),
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

instruction! { LoadGlobal LoadConst CallFunction StoreName LoadName DeleteName }
binary_op! { Add Sub Mul Div Mod }
