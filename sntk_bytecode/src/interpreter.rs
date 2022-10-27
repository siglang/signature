use crate::{
    builtin::get_builtin,
    code::{BinaryOp, BinaryOpEq, Instruction, UnaryOp},
    error::{ByteCodeRuntime, INVALID_OPERAND, NOT_A_BOOLEAN, NOT_A_FUNCTION, NOT_A_LITERAL_VALUE, NOT_DEFINED, UNKNOWN_FUNCTION},
    runtime_error,
    stack::{Environment, LiteralValue, Stack, StackTrait, Value},
};

/// Provides the basic methods of the bytecode interpreter.
pub trait InterpreterBase {
    fn new(instructions: Vec<Instruction>) -> Interpreter;
    fn new_with(instructions: Vec<Instruction>, stack: Stack, environment: Environment) -> Interpreter;
    fn run(&mut self);
}

/// Instructions implementation trait.
trait InstructionTrait {
    fn load_global(&mut self, name: String);
    fn load_const(&mut self, value: Value);
    fn load_name(&mut self, name: String);
    fn store_name(&mut self, name: String);
    fn call_function(&mut self, argc: usize);
    fn jump_if_true(&mut self, offset: usize);
    fn jump_if_false(&mut self, offset: usize);
    fn jump(&mut self, offset: usize);
    fn block(&mut self, instructions: Vec<Instruction>);
    fn return_value(&mut self);
}

/// Binary operator instructions implementation trait.
trait BinaryOpTrait {
    fn binary_op_add(&mut self);
    fn binary_op_sub(&mut self);
    fn binary_op_mul(&mut self);
    fn binary_op_div(&mut self);
    fn binary_op_mod(&mut self);
}

/// Binary operator equal instructions implementation trait.
trait BinaryOpEqTrait {
    fn binary_op_eq(&mut self);
    fn binary_op_neq(&mut self);
    fn binary_op_lt(&mut self);
    fn binary_op_gt(&mut self);
    fn binary_op_lt_eq(&mut self);
    fn binary_op_gt_eq(&mut self);
}

/// Unary operator instructions implementation trait.
trait UnaryOpTrait {
    fn unary_op_not(&mut self);
    fn unary_op_minus(&mut self);
}

/// **Virtual machine interpreter.** it works on a stack basis.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Interpreter {
    pub stack: Stack,
    pub instructions: Vec<Instruction>,
    pub instruction_pointer: usize,
    pub environment: Environment,
}

impl InstructionTrait for Interpreter {
    fn load_global(&mut self, name: String) {
        self.stack.push(Value::Identifier(name));
    }

    fn load_const(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn load_name(&mut self, name: String) {
        self.stack
            .push(self.environment.get(&name).unwrap_or_else(|| runtime_error!(self; NOT_DEFINED; name)));
    }

    fn store_name(&mut self, name: String) {
        let value = self.stack.pop();
        self.environment.set(name, value);
    }

    fn call_function(&mut self, argc: usize) {
        let function = self.stack.pop();

        match function {
            Value::Identifier(name) => match get_builtin(name.clone()) {
                Some(builtin) => {
                    let mut args = Vec::new();

                    for _ in 0..argc {
                        args.push(self.stack.pop());
                    }

                    args.reverse();

                    self.stack.push(Value::LiteralValue(builtin(
                        args.iter()
                            .map(|arg| LiteralValue::try_from(arg.clone()).unwrap_or_else(|value| runtime_error!(self; NOT_A_LITERAL_VALUE; value)))
                            .collect(),
                    )));
                }
                None => runtime_error!(self; UNKNOWN_FUNCTION; name),
            },
            _ => runtime_error!(self; NOT_A_FUNCTION; function),
        };
    }

    fn jump_if_true(&mut self, pos: usize) {
        if let Value::LiteralValue(LiteralValue::Boolean(b)) = self.stack.pop() {
            if b {
                self.instruction_pointer = pos;
            }
        } else {
            runtime_error!(self; NOT_A_BOOLEAN; self.stack.pop());
        }
    }

    fn jump_if_false(&mut self, pos: usize) {
        if let Value::LiteralValue(LiteralValue::Boolean(b)) = self.stack.pop() {
            if !b {
                self.instruction_pointer = pos;
            }
        } else {
            runtime_error!(self; NOT_A_BOOLEAN; self.stack.pop());
        }
    }

    fn jump(&mut self, pos: usize) {
        self.instruction_pointer = pos;
    }

    fn block(&mut self, instructions: Vec<Instruction>) {
        Interpreter::new_with(instructions, Stack::new(), Environment::new_with_parent(self.environment.clone())).run();
    }

    fn return_value(&mut self) {
        let value = self.stack.pop();
        self.stack.push(Value::Return(Box::new(value)));
    }
}

macro_rules! fn_binary_op {
    ($name:ident; $op:tt; $($type:ident)*) => {
        fn $name(&mut self) {
            let right = self.stack.pop();
            let left = self.stack.pop();

            match (left, right) {
                $(
                    (Value::LiteralValue(LiteralValue::$type(left)), Value::LiteralValue(LiteralValue::$type(right))) => {
                        self.stack.push(Value::LiteralValue(LiteralValue::$type(left + &right)));
                    }
                )*
                _ => runtime_error!(self; INVALID_OPERAND; stringify!($op)),
            }
        }
    }
}

macro_rules! fn_binary_op_eq {
    ($name:ident; $op:tt) => {
        fn $name(&mut self) {
            let right = self.stack.pop();
            let left = self.stack.pop();

            match (left, right) {
                (Value::LiteralValue(LiteralValue::Number(left)), Value::LiteralValue(LiteralValue::Number(right))) => {
                    self.stack.push(Value::LiteralValue(LiteralValue::Boolean(left $op right)))
                }
                _ => runtime_error!(self; INVALID_OPERAND; stringify!($op)),
            }
        }
    };
    (@eq $name:ident; $op:tt;) => {
        fn $name(&mut self) {
            let right = self.stack.pop();
            let left = self.stack.pop();

            self.stack.push(Value::LiteralValue(LiteralValue::Boolean(left $op right)));
        }
    }
}

macro_rules! fn_unary_op {
    ($name:ident; $op:tt; $type:ident) => {
        fn $name(&mut self) {
            let value = self.stack.pop();

            match value {
                Value::LiteralValue(LiteralValue::$type(value)) => self.stack.push(Value::LiteralValue(LiteralValue::$type($op value))),
                _ => runtime_error!(self; INVALID_OPERAND; stringify!($op)),
            }
        }
    }
}

impl BinaryOpTrait for Interpreter {
    fn_binary_op!(binary_op_add; +; Number String);
    fn_binary_op!(binary_op_sub; -; Number);
    fn_binary_op!(binary_op_mul; *; Number);
    fn_binary_op!(binary_op_div; /; Number);
    fn_binary_op!(binary_op_mod; %; Number);
}

impl BinaryOpEqTrait for Interpreter {
    fn_binary_op_eq!(@eq binary_op_eq; ==;);
    fn_binary_op_eq!(@eq binary_op_neq; !=;);
    fn_binary_op_eq!(binary_op_gt; >);
    fn_binary_op_eq!(binary_op_lt; <);
    fn_binary_op_eq!(binary_op_gt_eq; >=);
    fn_binary_op_eq!(binary_op_lt_eq; <=);
}

impl UnaryOpTrait for Interpreter {
    fn_unary_op!(unary_op_not; !; Boolean);
    fn_unary_op!(unary_op_minus; -; Number);
}

impl InterpreterBase for Interpreter {
    fn new(instructions: Vec<Instruction>) -> Self {
        Interpreter {
            stack: Stack::new(),
            instructions,
            instruction_pointer: 0,
            environment: Environment::new(),
        }
    }

    fn new_with(instructions: Vec<Instruction>, stack: Stack, environment: Environment) -> Interpreter {
        Interpreter {
            stack,
            instructions,
            instruction_pointer: 0,
            environment,
        }
    }

    fn run(&mut self) {
        while self.instruction_pointer < self.instructions.len() {
            match self.instructions[self.instruction_pointer].clone() {
                Instruction::LoadConst(value) => self.load_const(value),
                Instruction::LoadGlobal(name) => self.load_global(name),
                Instruction::LoadName(name) => self.load_name(name),
                Instruction::StoreName(name) => self.store_name(name),
                Instruction::CallFunction(argc) => self.call_function(argc),
                Instruction::JumpIfTrue(pos) => self.jump_if_true(pos),
                Instruction::JumpIfFalse(pos) => self.jump_if_false(pos),
                Instruction::Jump(pos) => self.jump(pos),
                Instruction::BinaryOp(op) => match op {
                    BinaryOp::Add => self.binary_op_add(),
                    BinaryOp::Sub => self.binary_op_sub(),
                    BinaryOp::Mul => self.binary_op_mul(),
                    BinaryOp::Div => self.binary_op_div(),
                    BinaryOp::Mod => self.binary_op_mod(),
                },
                Instruction::BinaryOpEq(op) => match op {
                    BinaryOpEq::Eq => self.binary_op_eq(),
                    BinaryOpEq::Neq => self.binary_op_neq(),
                    BinaryOpEq::Lt => self.binary_op_lt(),
                    BinaryOpEq::Gt => self.binary_op_gt(),
                    BinaryOpEq::Lte => self.binary_op_lt_eq(),
                    BinaryOpEq::Gte => self.binary_op_gt_eq(),
                },
                Instruction::UnaryOp(op) => match op {
                    UnaryOp::Not => self.unary_op_not(),
                    UnaryOp::Minus => self.unary_op_minus(),
                },
                Instruction::Block(block) => self.block(block),
                Instruction::Return => self.return_value(),
            }

            self.instruction_pointer += 1;
        }
    }
}
