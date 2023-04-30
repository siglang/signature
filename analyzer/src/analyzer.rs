#![allow(unused_variables)]

use crate::{
    symbol_table::{SymbolAttributes, SymbolEntry, SymbolKind, SymbolTable},
    type_checker::TypeChecker,
    SemanticError, SemanticErrorKind, SemanticResult,
};
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
            symbol_table: SymbolTable::new(None),
        }
    }

    pub fn analyze(&mut self) -> SemanticResult<Program> {
        for statement in self.program.statements.clone() {
            self.analyze_statement(&statement)?;
        }

        Ok(self.program.clone())
    }

    fn analyze_statement(&mut self, statement: &Statement) -> SemanticResult<()> {
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

    fn analyze_let_statement(&mut self, statement: &LetStatement) -> SemanticResult<()> {
        let type_checker = TypeChecker(self.symbol_table.clone());

        let expression_type = type_checker.typeof_expression(&statement.value)?;
        let type_annotation = type_checker.typeof_data_type(&statement.data_type)?;

        if expression_type != statement.data_type {
            return Err(SemanticError::new(
                SemanticErrorKind::TypeMismatch(
                    expression_type.to_string(),
                    type_annotation.to_string(),
                ),
                statement.position,
            ));
        }

        self.symbol_table.insert(
            &statement.identifier.value,
            SymbolEntry::new(
                statement.data_type.clone(),
                SymbolAttributes::default(),
                SymbolKind::Variable,
            ),
        );

        Ok(())
    }

    fn analyze_auto_statement(&mut self, statement: &AutoStatement) -> SemanticResult<()> {
        let type_checker = TypeChecker(self.symbol_table.clone());

        let expression_type = type_checker.typeof_expression(&statement.value)?;

        self.symbol_table.insert(
            &statement.identifier.value,
            SymbolEntry::new(
                expression_type.clone(),
                SymbolAttributes::default(),
                SymbolKind::Variable,
            ),
        );

        Ok(())
    }

    fn analyze_return_statement(&mut self, statement: &ReturnStatement) -> SemanticResult<()> {
        todo!()
    }

    fn analyze_type_statement(&mut self, statement: &TypeStatement) -> SemanticResult<()> {
        todo!()
    }

    fn analyze_declare_statement(&mut self, statement: &DeclareStatement) -> SemanticResult<()> {
        todo!()
    }

    fn analyze_struct_statement(&mut self, statement: &StructStatement) -> SemanticResult<()> {
        todo!()
    }

    fn analyze_expression(&mut self, expression: &ExpressionStatement) -> SemanticResult<()> {
        todo!()
    }
}
