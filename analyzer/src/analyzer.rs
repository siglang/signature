#![allow(unused_variables)]

use crate::{
    symbol_table::{SymbolAttributes, SymbolEntry, SymbolKind, SymbolTable},
    type_checker::TypeChecker,
    SemanticError, SemanticResult,
};
use parser::ast::{
    AutoStatement, DataType, DataTypeKind, DeclareStatement, Expression, LetStatement, Position,
    Program, ReturnStatement, Statement, StructStatement, TypeStatement,
};

/// # Analyzer
///
/// The analyzer is responsible for analyzing the AST, type checking and checking for semantic errors.
///
/// ## Example
///
/// ```ignore
/// use analyzer::Analyzer;
///
/// match Analyzer::new(program).analyze() {
///    Ok(_) => println!("No semantic errors found!"),
///    Err(error) => println!("Semantic error: {error}")
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Analyzer {
    pub program: Program,
    pub symbol_table: SymbolTable,
    pub return_type: DataTypeKind,
}

impl Analyzer {
    /// Creates a new analyzer with an empty symbol table.
    pub fn new(program: Program) -> Self {
        Self {
            program,
            symbol_table: SymbolTable::new(None),
            return_type: DataTypeKind::Unknown,
        }
    }

    /// Creates a new analyzer with the given symbol table.
    pub fn new_with_symbol_table(program: Program, symbol_table: SymbolTable) -> Self {
        Self {
            program,
            symbol_table,
            return_type: DataTypeKind::Unknown,
        }
    }

    fn type_checker(&self) -> TypeChecker {
        TypeChecker::new(self.symbol_table.clone())
    }

    fn type_checker_with_provided_type(&self, ttype: DataTypeKind) -> TypeChecker {
        TypeChecker::new_with_provided_type(self.symbol_table.clone(), ttype)
    }

    fn set_return_type(&mut self, ttype: DataTypeKind, position: Position) -> SemanticResult<()> {
        match self.return_type {
            DataTypeKind::Unknown => self.return_type = ttype,
            _ => {
                if self.return_type != ttype {
                    return Err(SemanticError::type_mismatch(
                        self.return_type.clone(),
                        ttype,
                        position,
                    ));
                }
            }
        }

        Ok(())
    }

    /// Analyzes the program and returns a `SemanticResult` with return type of the program.
    pub fn analyze(&mut self) -> SemanticResult<DataTypeKind> {
        for statement in self.program.clone() {
            self.analyze_statement(&statement)?;
        }

        Ok(self.return_type.clone())
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
        let type_annotation = self.type_checker().typeof_data_type(&statement.data_type)?;
        let expression_type = self.analyze_expression_with_provided_type(
            &statement.value,
            type_annotation.clone().kind,
        )?;

        if expression_type != type_annotation {
            return Err(SemanticError::type_mismatch(
                expression_type,
                type_annotation,
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
        let expression_type = self.analyze_expression(&statement.value)?;
        self.set_return_type(expression_type.kind, statement.position)?;

        Ok(())
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

    fn analyze_expression_with_provided_type(
        &mut self,
        expression: &Expression,
        ttype: DataTypeKind,
    ) -> SemanticResult<DataType> {
        self.type_checker_with_provided_type(ttype)
            .typeof_expression(expression)
    }
}
