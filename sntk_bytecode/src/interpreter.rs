use crate::{builtin::*, code::*, error::*, runtime_error, stack::*};
use std::collections::*;

/// Provides the basic methods of the bytecode interpreter.
pub trait InterpreterBase {
    fn new(instructions: Vec<Instruction>) -> Interpreter;
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
    pub environment: HashMap<String, Value>,
}

impl InstructionTrait for Interpreter {
    /// **Loads a global variable.**
    fn load_global(&mut self, name: String) {
        self.stack.push(Value::Identifier(name));
    }

    /// **Loads a constant.**
    fn load_const(&mut self, value: Value) {
        self.stack.push(value);
    }

    /// **Loads a name.**
    fn load_name(&mut self, name: String) {
        self.stack.push(self.environment.get(&name).unwrap().clone());
    }

    /// **Stores a name.**
    fn store_name(&mut self, name: String) {
        let value = self.stack.pop().unwrap();
        self.environment.insert(name, value);
    }

    /// **Calls a function.**
    fn call_function(&mut self, argc: usize) {
        let function = self.stack.pop().unwrap();
        let mut args = Vec::new();

        for _ in 0..argc {
            args.push(self.stack.pop().unwrap());
        }

        args.reverse();

        match function {
            Value::Identifier(name) => match get_builtin(name.clone()) {
                Some(builtin) => builtin(Some(args)),
                None => runtime_error!(self; UNKNOWN_FUNCTION; name),
            },
            _ => runtime_error!(self; NOT_A_FUNCTION; function),
        };
    }

    /// **Jumps if the top of the stack is true.**
    fn jump_if_true(&mut self, pos: usize) {
        if let Value::LiteralValue(LiteralValue::Boolean(b)) = self.stack.pop().unwrap() {
            if b {
                self.instruction_pointer = pos;
            }
        } else {
            runtime_error!(self; NOT_A_BOOLEAN; self.stack.pop().unwrap());
        }
    }

    /// **Jumps if the top of the stack is false.**
    fn jump_if_false(&mut self, pos: usize) {
        if let Value::LiteralValue(LiteralValue::Boolean(b)) = self.stack.pop().unwrap() {
            if !b {
                self.instruction_pointer = pos;
            }
        } else {
            runtime_error!(self; NOT_A_BOOLEAN; self.stack.pop().unwrap());
        }
    }

    /// **Jumps to a position.**
    fn jump(&mut self, pos: usize) {
        self.instruction_pointer = pos;
    }
}

impl BinaryOpTrait for Interpreter {
    fn binary_op_add(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::LiteralValue(LiteralValue::Number(left)), Value::LiteralValue(LiteralValue::Number(right))) => {
                self.stack.push(Value::LiteralValue(LiteralValue::Number(left + right)))
            }
            (Value::LiteralValue(LiteralValue::String(left)), Value::LiteralValue(LiteralValue::String(right))) => {
                self.stack.push(Value::LiteralValue(LiteralValue::String(left + &right)))
            }
            _ => runtime_error!(self; INVALID_OPERAND; "+"),
        }
    }

    fn binary_op_sub(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::LiteralValue(LiteralValue::Number(left)), Value::LiteralValue(LiteralValue::Number(right))) => {
                self.stack.push(Value::LiteralValue(LiteralValue::Number(left - right)))
            }
            _ => runtime_error!(self; INVALID_OPERAND; "-"),
        }
    }

    fn binary_op_mul(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::LiteralValue(LiteralValue::Number(left)), Value::LiteralValue(LiteralValue::Number(right))) => {
                self.stack.push(Value::LiteralValue(LiteralValue::Number(left * right)))
            }
            _ => runtime_error!(self; INVALID_OPERAND; "*"),
        }
    }

    fn binary_op_div(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::LiteralValue(LiteralValue::Number(left)), Value::LiteralValue(LiteralValue::Number(right))) => {
                self.stack.push(Value::LiteralValue(LiteralValue::Number(left / right)))
            }
            _ => runtime_error!(self; INVALID_OPERAND; "/"),
        }
    }

    fn binary_op_mod(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::LiteralValue(LiteralValue::Number(left)), Value::LiteralValue(LiteralValue::Number(right))) => {
                self.stack.push(Value::LiteralValue(LiteralValue::Number(left % right)))
            }
            _ => runtime_error!(self; INVALID_OPERAND; "%"),
        }
    }
}

impl BinaryOpEqTrait for Interpreter {
    fn binary_op_eq(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        self.stack.push(Value::LiteralValue(LiteralValue::Boolean(left == right)));
    }

    fn binary_op_neq(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        self.stack.push(Value::LiteralValue(LiteralValue::Boolean(left != right)));
    }

    fn binary_op_gt(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::LiteralValue(LiteralValue::Number(left)), Value::LiteralValue(LiteralValue::Number(right))) => {
                self.stack.push(Value::LiteralValue(LiteralValue::Boolean(left > right)))
            }
            _ => runtime_error!(self; INVALID_OPERAND; ">"),
        }
    }

    fn binary_op_lt(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::LiteralValue(LiteralValue::Number(left)), Value::LiteralValue(LiteralValue::Number(right))) => {
                self.stack.push(Value::LiteralValue(LiteralValue::Boolean(left < right)))
            }
            _ => runtime_error!(self; INVALID_OPERAND; "<"),
        }
    }

    fn binary_op_lt_eq(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::LiteralValue(LiteralValue::Number(left)), Value::LiteralValue(LiteralValue::Number(right))) => {
                self.stack.push(Value::LiteralValue(LiteralValue::Boolean(left <= right)))
            }
            _ => runtime_error!(self; INVALID_OPERAND; "<="),
        }
    }

    fn binary_op_gt_eq(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::LiteralValue(LiteralValue::Number(left)), Value::LiteralValue(LiteralValue::Number(right))) => {
                self.stack.push(Value::LiteralValue(LiteralValue::Boolean(left >= right)))
            }
            _ => runtime_error!(self; INVALID_OPERAND; ">="),
        }
    }
}

impl UnaryOpTrait for Interpreter {
    fn unary_op_not(&mut self) {
        let value = self.stack.pop().unwrap();

        match value {
            Value::LiteralValue(LiteralValue::Boolean(value)) => self.stack.push(Value::LiteralValue(LiteralValue::Boolean(!value))),
            _ => runtime_error!(self; INVALID_OPERAND; "!"),
        }
    }

    fn unary_op_minus(&mut self) {
        let value = self.stack.pop().unwrap();

        match value {
            Value::LiteralValue(LiteralValue::Number(value)) => self.stack.push(Value::LiteralValue(LiteralValue::Number(-value))),
            _ => runtime_error!(self; INVALID_OPERAND; "-"),
        }
    }
}

impl InterpreterBase for Interpreter {
    fn new(instructions: Vec<Instruction>) -> Self {
        Interpreter {
            stack: Stack::new(),
            instructions,
            instruction_pointer: 0,
            environment: HashMap::new(),
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
            }

            self.instruction_pointer += 1;
        }
    }
}
