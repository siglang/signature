use crate::{
    error::{CompileError, TypeError, EXPECTED_DATA_TYPE},
    helpers::{compile_block, last_instruction_data_type, literal_value, type_checked_array, type_checked_function},
    tc::{SharedTypeEnvironment, Type, TypeEnvironment, TypeTrait},
    type_error,
};
use sntk_core::{
    parser::ast::{
        AutoStatement, BlockExpression, BooleanLiteral, CallExpression, DataType, Expression, ExpressionStatement, FunctionLiteral, Identifier,
        IfExpression, IndexExpression, InfixExpression, LetStatement, NumberLiteral, PrefixExpression, Program, ReturnStatement, Statement,
        StringLiteral, TypeStatement, TypeofExpression,
    },
    tokenizer::token::Tokens,
};
use sntk_ir::{
    builtin::get_builtin,
    code::{BinaryOp, BinaryOpEq, Instruction, UnaryOp},
    interpreter::{Interpreter, InterpreterBase},
};
use std::{cell::RefCell, rc::Rc};

#[derive(Debug, Clone)]
pub struct Code(pub Vec<Instruction>);

impl Code {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn push_instruction(&mut self, instruction: &Instruction) {
        self.0.push(instruction.clone());
    }
}

#[derive(Debug)]
pub struct Compiler {
    pub program: Program,
    pub code: Code,
    pub type_environment: SharedTypeEnvironment,
}

pub type CompileResult<T> = Result<T, CompileError>;

pub trait CompilerTrait {
    fn new(program: Program) -> Self;
    fn new_with_type_environment(program: Program, type_environment: SharedTypeEnvironment) -> Self;
    fn compile_program(&mut self) -> CompileResult<Interpreter>;
    fn compile_let_statement(&mut self, let_statement: &LetStatement) -> CompileResult<()>;
    fn compile_auto_statement(&mut self, auto_statement: &AutoStatement) -> CompileResult<()>;
    fn compile_return_statement(&mut self, return_statement: &ReturnStatement) -> CompileResult<()>;
    fn compile_type_statement(&mut self, type_statement: &TypeStatement) -> CompileResult<()>;
    fn compile_expression(&mut self, expression: &Expression, data_type: Option<&DataType>) -> CompileResult<()>;
}

impl CompilerTrait for Compiler {
    fn new(program: Program) -> Self {
        Self {
            program,
            code: Code::new(),
            type_environment: Rc::new(RefCell::new(TypeEnvironment::new())),
        }
    }

    fn new_with_type_environment(program: Program, type_environment: SharedTypeEnvironment) -> Self {
        Self {
            program,
            code: Code::new(),
            type_environment,
        }
    }

    fn compile_program(&mut self) -> CompileResult<Interpreter> {
        if !self.program.errors.is_empty() {
            return Err(CompileError::ParsingError(self.program.errors.clone()));
        }

        for statement in self.program.statements.clone() {
            match statement {
                Statement::LetStatement(statement) => self.compile_let_statement(&statement)?,
                Statement::AutoStatement(statement) => self.compile_auto_statement(&statement)?,
                Statement::ReturnStatement(statement) => self.compile_return_statement(&statement)?,
                Statement::TypeStatement(statement) => self.compile_type_statement(&statement)?,
                Statement::StructStatement(_) => unimplemented!(),
                Statement::ExpressionStatement(ExpressionStatement { expression, .. }) => self.compile_expression(&expression, None)?,
            };
        }

        Ok(Interpreter::new(self.code.clone().0))
    }

    fn compile_let_statement(&mut self, let_statement: &LetStatement) -> CompileResult<()> {
        let LetStatement { name, value, data_type, .. } = let_statement;

        self.compile_expression(value, Some(data_type))?;

        if let Some(data_type) = last_instruction_data_type(&self.code.0, &let_statement.position, Rc::clone(&self.type_environment))? {
            self.type_environment.borrow_mut().set(name.value.clone(), data_type);
        }

        self.code.push_instruction(&Instruction::StoreName(name.clone().value));

        Ok(())
    }

    fn compile_auto_statement(&mut self, auto_statement: &AutoStatement) -> CompileResult<()> {
        let AutoStatement { name, value, .. } = auto_statement;

        self.compile_expression(value, None)?;

        if let Some(data_type) = last_instruction_data_type(&self.code.0, &auto_statement.position, Rc::clone(&self.type_environment))? {
            self.type_environment.borrow_mut().set(name.value.clone(), data_type);
        }

        self.code.push_instruction(&Instruction::StoreName(name.clone().value));

        Ok(())
    }

    fn compile_return_statement(&mut self, return_statement: &ReturnStatement) -> CompileResult<()> {
        let ReturnStatement { value, .. } = return_statement;

        self.compile_expression(value, None)?;
        self.code.push_instruction(&Instruction::Return);

        Ok(())
    }

    fn compile_type_statement(&mut self, _type_statement: &TypeStatement) -> CompileResult<()> {
        todo!()
    }

    fn compile_expression(&mut self, expression: &Expression, data_type: Option<&DataType>) -> CompileResult<()> {
        macro_rules! match_type {
            ($type:expr; $e:expr; $pos:expr;) => {
                let data_type = match data_type {
                    Some(data_type) => data_type.clone(),
                    None => Type::get_data_type_from_expression($e, &$pos, Rc::clone(&self.type_environment))?,
                };

                if !Type(data_type.clone()).eq_from_type(&Type($type)) {
                    return Err(type_error! { EXPECTED_DATA_TYPE; data_type, $type; $pos; });
                }
            };
        }

        match expression {
            Expression::BlockExpression(BlockExpression { statements, position }) => {
                let block = compile_block(statements.clone(), position, Rc::clone(&self.type_environment))?;

                if let Some(data_type) = data_type {
                    if !Type(block.clone().1).eq_from_type(&Type(data_type.clone())) {
                        return Err(type_error! { EXPECTED_DATA_TYPE; data_type, block.1; position.clone(); });
                    }
                }

                self.code.push_instruction(&Instruction::Block(block.0));

                Ok(())
            }

            Expression::Identifier(Identifier { value, .. }) => {
                self.code.push_instruction(&Instruction::LoadName(value.clone()));

                Ok(())
            }

            Expression::NumberLiteral(NumberLiteral { position, .. }) => {
                match_type! { DataType::Number; expression; position.clone(); };

                self.code.push_instruction(&Instruction::LoadConst(literal_value(
                    expression.clone(),
                    Rc::clone(&self.type_environment),
                )?));

                Ok(())
            }

            Expression::StringLiteral(StringLiteral { position, .. }) => {
                match_type! { DataType::String; expression; position.clone(); };

                self.code.push_instruction(&Instruction::LoadConst(literal_value(
                    expression.clone(),
                    Rc::clone(&self.type_environment),
                )?));

                Ok(())
            }

            Expression::BooleanLiteral(BooleanLiteral { position, .. }) => {
                match_type! { DataType::Boolean; expression; position.clone(); };

                self.code.push_instruction(&Instruction::LoadConst(literal_value(
                    expression.clone(),
                    Rc::clone(&self.type_environment),
                )?));

                Ok(())
            }

            Expression::ArrayLiteral(expression) => {
                self.code.push_instruction(&Instruction::LoadConst(type_checked_array(
                    expression,
                    data_type,
                    Rc::clone(&self.type_environment),
                )?));

                Ok(())
            }

            Expression::FunctionLiteral(expression) => {
                self.code.push_instruction(&Instruction::LoadConst(type_checked_function(
                    expression,
                    data_type,
                    Rc::clone(&self.type_environment),
                )?));

                Ok(())
            }

            Expression::StructLiteral(_) => {
                todo!()
            }

            Expression::PrefixExpression(PrefixExpression { operator, right, .. }) => {
                self.compile_expression(right, None)?;

                match operator {
                    Tokens::Minus => self.code.push_instruction(&Instruction::UnaryOp(UnaryOp::Minus)),
                    Tokens::Bang => self.code.push_instruction(&Instruction::UnaryOp(UnaryOp::Not)),
                    _ => panic!("Unknown operator: {}", operator),
                }

                Ok(())
            }

            Expression::InfixExpression(InfixExpression {
                left,
                operator,
                right,
                position,
            }) => {
                let left_data_type = Type::get_data_type_from_expression(left, position, Rc::clone(&self.type_environment))?;
                let right_data_type = Type::get_data_type_from_expression(right, position, Rc::clone(&self.type_environment))?;

                self.compile_expression(left, Some(&right_data_type))?;
                self.compile_expression(right, Some(&left_data_type))?;

                if let Some(data_type) = data_type {
                    if !Type(data_type.clone()).eq_from_type(&Type(left_data_type.clone())) {
                        return Err(type_error! { EXPECTED_DATA_TYPE; data_type, left_data_type; position.clone(); });
                    }
                }

                match operator {
                    Tokens::Plus => self.code.push_instruction(&Instruction::BinaryOp(BinaryOp::Add)),
                    Tokens::Minus => self.code.push_instruction(&Instruction::BinaryOp(BinaryOp::Sub)),
                    Tokens::Asterisk => self.code.push_instruction(&Instruction::BinaryOp(BinaryOp::Mul)),
                    Tokens::Slash => self.code.push_instruction(&Instruction::BinaryOp(BinaryOp::Div)),
                    Tokens::Percent => self.code.push_instruction(&Instruction::BinaryOp(BinaryOp::Mod)),
                    Tokens::EQ => self.code.push_instruction(&Instruction::BinaryOpEq(BinaryOpEq::Eq)),
                    Tokens::NEQ => self.code.push_instruction(&Instruction::BinaryOpEq(BinaryOpEq::Neq)),
                    Tokens::LT => self.code.push_instruction(&Instruction::BinaryOpEq(BinaryOpEq::Lt)),
                    Tokens::GT => self.code.push_instruction(&Instruction::BinaryOpEq(BinaryOpEq::Gt)),
                    Tokens::LTE => self.code.push_instruction(&Instruction::BinaryOpEq(BinaryOpEq::Lte)),
                    Tokens::GTE => self.code.push_instruction(&Instruction::BinaryOpEq(BinaryOpEq::Gte)),
                    _ => panic!(),
                }

                Ok(())
            }

            Expression::CallExpression(CallExpression { function, arguments, .. }) => {
                for argument in arguments.clone() {
                    self.compile_expression(&argument, None)?;
                }

                match *function.clone() {
                    Expression::Identifier(Identifier { value, .. }) => match get_builtin(value.clone()) {
                        Some(_) => self.code.push_instruction(&Instruction::LoadGlobal(value)),
                        None => self.code.push_instruction(&Instruction::LoadName(value)),
                    },
                    Expression::FunctionLiteral(FunctionLiteral { .. }) | Expression::CallExpression(CallExpression { .. }) => {
                        self.compile_expression(function, None)?;
                    }
                    expression => panic!("Unknown function: {:?}", expression),
                }

                self.code.push_instruction(&Instruction::CallFunction(arguments.len()));

                Ok(())
            }

            Expression::IndexExpression(IndexExpression { .. }) => {
                todo!()
            }

            Expression::IfExpression(IfExpression {
                condition,
                consequence,
                alternative,
                position,
            }) => {
                self.compile_expression(condition, None)?;

                self.code.push_instruction(&Instruction::If(
                    compile_block(consequence.statements.clone(), position, Rc::clone(&self.type_environment))?.0,
                    alternative
                        .clone()
                        .map(|expression| compile_block(expression.statements.clone(), position, Rc::clone(&self.type_environment)))
                        .transpose()?
                        .map(|block| block.0),
                ));

                Ok(())
            }

            Expression::TypeofExpression(TypeofExpression { expression, position }) => {
                self.compile_expression(
                    &Expression::StringLiteral(StringLiteral {
                        value: Type::get_data_type_from_expression(expression, position, Rc::clone(&self.type_environment))?.to_string(),
                        position: position.clone(),
                    }),
                    None,
                )?;

                Ok(())
            }
        }
    }
}
