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
}

/// Binary operator instructions implementation trait.
trait BinaryOpTrait {
    fn binary_op_add(&mut self);
    fn binary_op_sub(&mut self);
    fn binary_op_mul(&mut self);
    fn binary_op_div(&mut self);
    fn binary_op_mod(&mut self);
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
///         /* Line 2 */ Instruction::LoadGlobal(0), // print
///         /* Line 2 */ Instruction::CallFunction(1), // print(7)
///         /* Line 3 */ Instruction::LoadName(0), // a, 7
///         /* Line 3 */ Instruction::LoadConst(2), // 10
///         /* Line 3 */ Instruction::BinaryOp(BinaryOp::Mul), // 7 * 10
///         /* Line 3 */ Instruction::StoreName(1), // b = 70
///         /* Line 4 */ Instruction::LoadName(1), // b, 70
///         /* Line 4 */ Instruction::LoadName(0), // a, 7
///         /* Line 4 */ Instruction::BinaryOp(BinaryOp::Add), // 70 + 7
///         /* Line 4 */ Instruction::LoadGlobal(0), // print
///         /* Line 4 */ Instruction::CallFunction(1), // print(77)
///     ],
///     vec![
///         Value::LiteralValue(LiteralValue::Number(5.0)),
///         Value::LiteralValue(LiteralValue::Number(2.0)),
///         Value::LiteralValue(LiteralValue::Number(10.0)),
///     ],
///     vec!["print".to_string(), "a".to_string(), "b".to_string()],
/// )
/// .run();
/// ```
#[derive(Debug, PartialEq, Clone)]
pub struct Interpreter {
    stack: Stack,
    instructions: Vec<Instruction>,
    instruction_pointer: usize,
    environment: HashMap<String, Value>,
    constants: Vec<Value>,
    names: Vec<String>,
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

        match function {
            Value::Identifier(name) => {
                if name == "print" {
                    println!("{:?}", args[0]);
                } else {
                    runtime_error!(self; UNKNOWN_FUNCTION; name);
                }
            }
            _ => runtime_error!(self; NOT_A_FUNCTION; function),
        }
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
                Instruction::BinaryOp(op) => match op {
                    BinaryOp::Add => self.binary_op_add(),
                    BinaryOp::Sub => self.binary_op_sub(),
                    BinaryOp::Mul => self.binary_op_mul(),
                    BinaryOp::Div => self.binary_op_div(),
                    BinaryOp::Mod => self.binary_op_mod(),
                },
            }

            self.instruction_pointer += 1;
        }
    }
}
