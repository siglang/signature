#![allow(unused_variables)]

use crate::{
    symbol_table::{SymbolAttributes, SymbolEntry, SymbolKind, SymbolTable},
    type_checker::TypeChecker,
    SemanticError, SemanticResult,
};
use parser::ast::{
    AutoStatement, DataType, DeclareStatement, Expression, LetStatement, Program, ReturnStatement,
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

    pub fn new_with_symbol_table(program: Program, symbol_table: SymbolTable) -> Self {
        Self {
            program,
            symbol_table,
        }
    }

    fn type_checker(&self) -> TypeChecker {
        TypeChecker(self.symbol_table.clone())
    }

    pub fn analyze(&mut self) -> SemanticResult<()> {
        for statement in self.program.clone() {
            self.analyze_statement(&statement)?;
        }

        Ok(())
    }

    fn analyze_statement(&mut self, statement: &Statement) -> SemanticResult<()> {
        match statement {
            Statement::LetStatement(statement) => self.analyze_let_statement(statement),
            Statement::AutoStatement(statement) => self.analyze_auto_statement(statement),
            Statement::ReturnStatement(statement) => self.analyze_return_statement(statement),
            Statement::TypeStatement(statement) => self.analyze_type_statement(statement),
            Statement::DeclareStatement(statement) => self.analyze_declare_statement(statement),
            Statement::StructStatement(statement) => self.analyze_struct_statement(statement),
            Statement::ExpressionStatement(expression) => {
                let debug = self.analyze_expression(&expression.expression)?;

                println!("{:?}", debug);

                Ok(())
            }
        }
    }

    fn analyze_let_statement(&mut self, statement: &LetStatement) -> SemanticResult<()> {
        let expression_type = self.analyze_expression(&statement.value)?;
        let type_annotation = self.type_checker().typeof_data_type(&statement.data_type)?;

        if expression_type != type_annotation {
            return Err(SemanticError::type_mismatch(
                expression_type.to_string(),
                type_annotation.to_string(),
                statement.position,
            ));
        }

        self.symbol_table
            .insert(
                &statement.identifier.value,
                SymbolEntry::new(
                    statement.data_type.clone(),
                    SymbolAttributes::default(),
                    SymbolKind::Variable,
                ),
            )
            .ok_or_else(|| {
                SemanticError::identifier_already_defined(
                    statement.identifier.value.clone(),
                    statement.position,
                )
            })?;

        Ok(())
    }

    fn analyze_auto_statement(&mut self, statement: &AutoStatement) -> SemanticResult<()> {
        let expression_type = self.analyze_expression(&statement.value)?;

        self.symbol_table
            .insert(
                &statement.identifier.value,
                SymbolEntry::new(
                    expression_type,
                    SymbolAttributes::default(),
                    SymbolKind::Variable,
                ),
            )
            .ok_or_else(|| {
                SemanticError::identifier_already_defined(
                    statement.identifier.value.clone(),
                    statement.position,
                )
            })?;

        Ok(())
    }

    fn analyze_return_statement(&mut self, statement: &ReturnStatement) -> SemanticResult<()> {
        todo!()
    }

    fn analyze_type_statement(&mut self, statement: &TypeStatement) -> SemanticResult<()> {
        let ttype = self.type_checker().typeof_data_type(&statement.data_type)?;

        self.symbol_table
            .insert(
                &statement.identifier.value,
                SymbolEntry::new(ttype, SymbolAttributes::default(), SymbolKind::Named),
            )
            .ok_or_else(|| {
                SemanticError::type_alias_already_defined(
                    statement.identifier.value.clone(),
                    statement.position,
                )
            })?;

        Ok(())
    }

    fn analyze_declare_statement(&mut self, statement: &DeclareStatement) -> SemanticResult<()> {
        let ttype = self.type_checker().typeof_data_type(&statement.data_type)?;

        self.symbol_table
            .insert(
                &statement.identifier.value,
                SymbolEntry::new(ttype, SymbolAttributes::default(), SymbolKind::Variable),
            )
            .ok_or_else(|| {
                SemanticError::identifier_already_defined(
                    statement.identifier.value.clone(),
                    statement.position,
                )
            })?;

        Ok(())
    }

    fn analyze_struct_statement(&mut self, statement: &StructStatement) -> SemanticResult<()> {
        todo!()
    }

    fn analyze_expression(&mut self, expression: &Expression) -> SemanticResult<DataType> {
        self.type_checker().typeof_expression(expression)
    }
}
