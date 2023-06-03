pub mod analyzer;
pub mod symbol_table;

use parser::ast::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub position: Position,
}

impl SemanticError {
    pub fn new(kind: SemanticErrorKind, position: Position) -> Self {
        Self { kind, position }
    }

    pub fn type_mismatch<T>(left: T, right: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            SemanticErrorKind::TypeMismatch(left.to_string(), right.to_string()),
            position,
        )
    }

    pub fn identifier_not_defined<T>(identifier: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            SemanticErrorKind::IdentifierNotDefined(identifier.to_string()),
            position,
        )
    }

    pub fn identifier_already_defined<T>(identifier: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            SemanticErrorKind::IdentifierAlreadyDefined(identifier.to_string()),
            position,
        )
    }

    pub fn type_alias_not_defined<T>(type_alias: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            SemanticErrorKind::TypeAliasNotDefined(type_alias.to_string()),
            position,
        )
    }

    pub fn type_alias_already_defined<T>(type_alias: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            SemanticErrorKind::TypeAliasAlreadyDefined(type_alias.to_string()),
            position,
        )
    }

    pub fn operator_not_supported<O, D>(operator: O, data_type: D, position: Position) -> Self
    where
        O: ToString,
        D: ToString,
    {
        Self::new(
            SemanticErrorKind::OperatorNotSupported(operator.to_string(), data_type.to_string()),
            position,
        )
    }

    pub fn type_annotation_needed(position: Position) -> Self {
        Self::new(SemanticErrorKind::TypeAnnotationNeeded, position)
    }

    pub fn identifier_not_mutable<T>(identifier: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            SemanticErrorKind::CannotAssignToImmutableVariable(identifier.to_string()),
            position,
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum SemanticErrorKind {
    TypeMismatch(String, String),
    IdentifierNotDefined(String),
    TypeAliasNotDefined(String),
    IdentifierAlreadyDefined(String),
    TypeAliasAlreadyDefined(String),
    OperatorNotSupported(String, String),
    TypeAnnotationNeeded,
    CannotAssignToImmutableVariable(String),
}

pub type SemanticResult<T> = Result<T, SemanticError>;
