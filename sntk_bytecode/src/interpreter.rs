use crate::{code::*, error::*, runtime_error, stack::*};
use std::collections::*;

/// Provides the basic methods of the bytecode interpreter.
pub trait InterpreterBase {
    fn new(instructions: Vec<Instruction>, constants: Vec<Value>, names: Vec<String>) -> Interpreter;
    fn run(&mut self);
}

/// Instructions implementation trait.
trait InstructionTrait {
    fn load_global(&mut self, pos: usize);
    fn load_const(&mut self, pos: usize);
    fn load_name(&mut self, pos: usize);
    fn store_name(&mut self, pos: usize);
    fn delete_name(&mut self, pos: usize);
    fn call_function(&mut self, pos: usize);
    fn jump_if_true(&mut self, pos: usize);
    fn jump_if_false(&mut self, pos: usize);
    fn jump(&mut self, pos: usize);
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
///
/// for example:
/// ```rust
/// use sntk_bytecode::{code::*, interpreter::*, stack::*};
///
/// Interpreter::new(
///     vec![
///         /* Line 1 */ Instruction::LoadConst(0), // 5
///         /* Line 1 */ Instruction::LoadConst(1), // 2
///         /* Line 1 */ Instruction::BinaryOp(BinaryOp::Add), // 5 + 2
///         /* Line 1 */ Instruction::StoreName(0), // a = 7
///         /* Line 2 */ Instruction::LoadName(0), // a, 7
///         /* Line 2 */ Instruction::LoadGlobal(1), // print
///         /* Line 2 */ Instruction::CallFunction(1), // print(7)
///         /* Line 3 */ Instruction::LoadName(0), // a, 7
///         /* Line 3 */ Instruction::LoadConst(2), // 10
///         /* Line 3 */ Instruction::LoadConst(3), // 20
///         /* Line 3 */ Instruction::LoadConst(4), // "hello"
///         /* Line 3 */ Instruction::LoadConst(5), // "world"
///         /* Line 3 */ Instruction::BinaryOp(BinaryOp::Add), // "hello" + "world"
///         /* Line 3 */ Instruction::LoadGlobal(2), // foo
///         /* Line 3 */ Instruction::CallFunction(4), // foo(7, 10, 20, "helloworld")
///         /* Line 4 */ Instruction::LoadName(0), // a, 7
///         /* Line 4 */ Instruction::LoadConst(1), // 2
///         /* Line 4 */ Instruction::BinaryOp(BinaryOp::Add), // 7 + 2
///         /* Line 4 */ Instruction::LoadConst(6), // 2
///         /* Line 4 */ Instruction::BinaryOp(BinaryOp::Mul), // (7 + 2) * 2
///         /* Line 4 */ Instruction::StoreName(0), // a = 16
///         /* Line 5 */ Instruction::LoadName(0), // a, 16
///         /* Line 5 */ Instruction::LoadGlobal(1), // print
///         /* Line 5 */ Instruction::CallFunction(1), // print(16)
///     ],
///     vec![
///         Value::LiteralValue(LiteralValue::Number(5.0)),
///         Value::LiteralValue(LiteralValue::Number(2.0)),
///         Value::LiteralValue(LiteralValue::Number(10.0)),
///         Value::LiteralValue(LiteralValue::Number(20.0)),
///         Value::LiteralValue(LiteralValue::String("hello".to_string())),
///         Value::LiteralValue(LiteralValue::String("world".to_string())),
///         Value::LiteralValue(LiteralValue::Number(2.0)),
///     ],
///     vec![
///         "a".to_string(),
///         "print".to_string(),
///         "foo".to_string(),
///     ],
/// ).run()
/// ```
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Interpreter {
    pub stack: Stack,
    pub instructions: Vec<Instruction>,
    pub instruction_pointer: usize,
    pub environment: HashMap<String, Value>,
    pub constants: Vec<Value>,
    pub names: Vec<String>,
}

impl InstructionTrait for Interpreter {
    /// **Loads a global variable.**
    fn load_global(&mut self, pos: usize) {
        self.stack.push(Value::Identifier(self.names[pos].clone()));
    }

    /// **Loads a constant.**
    fn load_const(&mut self, pos: usize) {
        self.stack.push(self.constants[pos].clone());
    }

    /// **Loads a name.**
    fn load_name(&mut self, pos: usize) {
        self.stack.push(self.environment[&self.names[pos]].clone());
    }

    /// **Stores a name.**
    fn store_name(&mut self, pos: usize) {
        self.environment.insert(self.names[pos].clone(), self.stack.pop().unwrap());
    }

    /// **Deletes a name.**
    fn delete_name(&mut self, pos: usize) {
        self.environment.remove(&self.names[pos]);
    }

    /// **Calls a function.**
    fn call_function(&mut self, pos: usize) {
        let function = self.stack.pop().unwrap();
        let mut args = Vec::new();

        for _ in 0..pos {
            args.push(self.stack.pop().unwrap());
        }

        args.reverse();

        match function {
            Value::Identifier(name) => match name.as_str() {
                "print" => println!("{:?}", args[0]),
                "foo" => println!("foo: {:?}", args),
                _ => runtime_error!(self; UNKNOWN_FUNCTION; name),
            },
            _ => runtime_error!(self; NOT_A_FUNCTION; function),
        }
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
    fn new(instructions: Vec<Instruction>, constants: Vec<Value>, names: Vec<String>) -> Self {
        Interpreter {
            stack: Stack::new(),
            instructions,
            instruction_pointer: 0,
            environment: HashMap::new(),
            constants,
            names,
        }
    }

    fn run(&mut self) {
        while self.instruction_pointer < self.instructions.len() {
            match self.instructions[self.instruction_pointer].clone() {
                Instruction::LoadConst(pos) => self.load_const(pos),
                Instruction::LoadGlobal(pos) => self.load_global(pos),
                Instruction::LoadName(pos) => self.load_name(pos),
                Instruction::StoreName(pos) => self.store_name(pos),
                Instruction::DeleteName(pos) => self.delete_name(pos),
                Instruction::CallFunction(pos) => self.call_function(pos),
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
