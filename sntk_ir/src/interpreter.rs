use crate::{
    builtin::get_builtin,
    code::{BinaryOp, BinaryOpEq, Block, Instruction, UnaryOp},
    error::{
        IrRuntime, INVAILD_ARGUMENTS, INVALID_OPERAND, NOT_A_BOOLEAN, NOT_A_FUNCTION,
        NOT_A_LITERAL_VALUE, NOT_DEFINED,
    },
    runtime_error,
    stack::{Environment, Stack, StackTrait},
    value::{LiteralValue, Value},
};

/// Provides the basic methods of the ir interpreter.
pub trait InterpreterBase {
    fn new(instructions: Vec<Instruction>) -> Self;
    fn new_with(instructions: Vec<Instruction>, stack: Stack, environment: Environment) -> Self;
    fn run(&mut self);
}

/// Instructions implementation trait.
trait InstructionTrait {
    fn load_global(&mut self, name: String);
    fn load_const(&mut self, value: Value);
    fn load_name(&mut self, name: String);
    fn store_name(&mut self, name: String);
    fn call_function(&mut self, argc: usize);
    fn if_else(&mut self, consequent: Block, alternative: Option<Block>);
    fn block(&mut self, instructions: Block);
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
        self.stack.push(
            self.environment
                .get(&name)
                .unwrap_or_else(|| runtime_error!(self; NOT_DEFINED; name)),
        );
    }

    fn store_name(&mut self, name: String) {
        let value = self.stack.pop();
        self.environment.set(name, value);
    }

    fn call_function(&mut self, argc: usize) {
        let function = self.stack.pop();

        let mut arguments = Vec::new();

        for _ in 0..argc {
            arguments.push(self.stack.pop());
        }

        arguments.reverse();

        match function {
            Value::Identifier(name) => {
                if let Some(builtin) = get_builtin(name) {
                    self.stack.push(Value::LiteralValue(builtin(
                        arguments
                            .iter()
                            .map(|arg| {
                                LiteralValue::try_from(arg.clone()).unwrap_or_else(
                                    |value| runtime_error!(self; NOT_A_LITERAL_VALUE; value),
                                )
                            })
                            .collect(),
                    )));
                }
            }
            Value::LiteralValue(LiteralValue::Function { parameters, body }) => {
                if parameters.len() != argc {
                    runtime_error!(self; INVAILD_ARGUMENTS; format!("expected {} arguments, got {}", parameters.len(), argc));
                }

                for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
                    self.environment.set(parameter.0.clone(), argument.clone());
                }

                let mut interpreter = Interpreter::new_with(
                    body.0,
                    self.stack.clone(),
                    Environment::new_with_parent(self.environment.clone()),
                );
                interpreter.run();

                if let Some(Value::Return(value)) = interpreter.stack.pop_option() {
                    self.stack.push(*value);
                } else {
                    self.stack
                        .push(Value::LiteralValue(LiteralValue::Boolean(true)));
                }
            }
            _ => runtime_error!(self; NOT_A_FUNCTION; function),
        };
    }

    fn if_else(&mut self, consequent: Block, alternative: Option<Block>) {
        let condition = self.stack.pop();

        match condition {
            Value::LiteralValue(LiteralValue::Boolean(condition)) => {
                if condition {
                    self.block(consequent);
                } else if let Some(alternative) = alternative {
                    self.block(alternative);
                }
            }
            _ => runtime_error!(self; NOT_A_BOOLEAN; condition),
        }
    }

    fn block(&mut self, instructions: Block) {
        let mut ins = Vec::new();

        for instruction in instructions.0 {
            match instruction {
                Instruction::Return => {
                    ins.push(instruction);
                    break;
                }
                _ => ins.push(instruction),
            }
        }

        let mut interpreter = Interpreter::new_with(
            ins,
            Stack::new(),
            Environment::new_with_parent(self.environment.clone()),
        );
        interpreter.run();

        let value = interpreter.stack.pop();

        match value {
            Value::Return(value) => self.stack.push(*value),
            Value::Identifier(_) => panic!("Return value must be a literal value"),
            _ => self.stack.push(value),
        };
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
                        self.stack.push(Value::LiteralValue(LiteralValue::$type(left $op &right)));
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

    fn new_with(
        instructions: Vec<Instruction>,
        stack: Stack,
        environment: Environment,
    ) -> Interpreter {
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
                Instruction::If(consequent, alternative) => self.if_else(consequent, alternative),
            }

            self.instruction_pointer += 1;
        }
    }
}
