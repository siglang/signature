#![allow(unused_variables)]

use crate::symbol_table::SymbolTable;
use parser::ast::{
    AutoStatement, DeclareStatement, ExpressionStatement, LetStatement, Program, ReturnStatement,
    Statement, StructStatement, TypeStatement,
};

#[derive(Debug, Clone)]
pub struct Analyzer {
    pub program: Program,
    pub symbol_table: SymbolTable,
}

impl Analyzer {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            symbol_table: SymbolTable::default(),
        }
    }

    pub fn analyze(&mut self) -> Program {
        for statement in self.program.statements.clone() {
            self.analyze_statement(&statement);
        }

        self.program.clone()
    }

    fn analyze_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::LetStatement(statement) => self.analyze_let_statement(statement),
            Statement::AutoStatement(statement) => self.analyze_auto_statement(statement),
            Statement::ReturnStatement(statement) => self.analyze_return_statement(statement),
            Statement::TypeStatement(statement) => self.analyze_type_statement(statement),
            Statement::DeclareStatement(statement) => self.analyze_declare_statement(statement),
            Statement::StructStatement(statement) => self.analyze_struct_statement(statement),
            Statement::ExpressionStatement(expression) => self.analyze_expression(expression),
        }
    }

    fn analyze_let_statement(&mut self, let_statement: &LetStatement) {
        todo!()
    }

    fn analyze_auto_statement(&mut self, auto_statement: &AutoStatement) {
        todo!()
    }

    fn analyze_return_statement(&mut self, return_statement: &ReturnStatement) {
        todo!()
    }

    fn analyze_type_statement(&mut self, type_statement: &TypeStatement) {
        todo!()
    }

    fn analyze_declare_statement(&mut self, declare_statement: &DeclareStatement) {
        todo!()
    }

    fn analyze_struct_statement(&mut self, struct_statement: &StructStatement) {
        todo!()
    }

    fn analyze_expression(&mut self, expression: &ExpressionStatement) {
        todo!()
    }
}
