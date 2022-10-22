use crate::{code::*, stack::*};
use std::collections::*;

// instruction! { LoadGlobal LoadConst CallFunction StoreName LoadName DeleteName }
// binary_op! { Add Sub Mul Div Mod }

#[derive(Debug, PartialEq, Clone)]
pub struct Interpreter {
    stack: Stack,
    instructions: Vec<Instruction>,
    instruction_pointer: usize,
    environment: HashMap<String, Value>, /* temporary */
    constants: Vec<Value>,
    names: Vec<String>,
}

impl Interpreter {
    pub fn load_global(&mut self, pos: usize) {
        self.stack.push(Value::Identifier(self.names[pos].clone()));
    }

    pub fn load_const(&mut self, pos: usize) {
        self.stack.push(self.constants[pos].clone());
    }

    pub fn load_name(&mut self, pos: usize) {
        self.stack.push(self.environment[&self.names[pos]].clone());
    }

    pub fn store_name(&mut self, pos: usize) {
        self.environment.insert(self.names[pos].clone(), self.stack.pop().unwrap());
    }

    pub fn delete_name(&mut self, pos: usize) {
        self.environment.remove(&self.names[pos]);
    }

    pub fn call_function(&mut self, pos: usize) {
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
                    panic!("Unknown function: {}", name);
                }
            }
            _ => panic!("Not a function: {:?}", function),
        }
    }

    pub fn add(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::Number(left), Value::Number(right)) => self.stack.push(Value::Number(left + right)),
            (Value::Str(left), Value::Str(right)) => self.stack.push(Value::Str(left + &right)),
            _ => panic!("Invalid operands for +"),
        }
    }

    pub fn sub(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::Number(left), Value::Number(right)) => self.stack.push(Value::Number(left - right)),
            _ => panic!("Invalid operands for -"),
        }
    }

    pub fn mul(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::Number(left), Value::Number(right)) => self.stack.push(Value::Number(left * right)),
            _ => panic!("Invalid operands for *"),
        }
    }

    pub fn div(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::Number(left), Value::Number(right)) => self.stack.push(Value::Number(left / right)),
            _ => panic!("Invalid operands for /"),
        }
    }

    pub fn mod_(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (left, right) {
            (Value::Number(left), Value::Number(right)) => self.stack.push(Value::Number(left % right)),
            _ => panic!("Invalid operands for %"),
        }
    }
}

impl Interpreter {
    pub fn new(instructions: Vec<Instruction>, constants: Vec<Value>, names: Vec<String>) -> Self {
        Interpreter {
            stack: Stack::new(),
            instructions,
            instruction_pointer: 0,
            environment: HashMap::new(),
            constants,
            names,
        }
    }

    pub fn run(&mut self) {
        while self.instruction_pointer < self.instructions.len() {
            let op = self.instructions[self.instruction_pointer].clone();

            match op {
                Instruction::LoadConst(pos) => self.load_const(pos),
                Instruction::LoadGlobal(pos) => self.load_global(pos),
                Instruction::LoadName(pos) => self.load_name(pos),
                Instruction::StoreName(pos) => self.store_name(pos),
                Instruction::DeleteName(pos) => self.delete_name(pos),
                Instruction::CallFunction(pos) => self.call_function(pos),
                Instruction::BinaryOp(op) => match op {
                    BinaryOp::Add => self.add(),
                    BinaryOp::Sub => self.sub(),
                    BinaryOp::Mul => self.mul(),
                    BinaryOp::Div => self.div(),
                    BinaryOp::Mod => self.mod_(),
                },
            }

            self.instruction_pointer += 1;
        }
    }
}
