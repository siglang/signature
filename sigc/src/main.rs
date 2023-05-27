#[macro_use]
extern crate lazy_static;

mod arguments;
mod evaluator;

use analyzer::{analyzer::Analyzer, SemanticError};
use clap::Parser as _;
use evaluator::{EvaluateError, Evaluator};
use parser::{ast::Position, tokenizer::Lexer, Parser, ParsingError};
use std::{fmt, fs};

#[derive(Debug, Clone)]
pub struct ErrorInfo {
    pub message: String,
    pub code: i32,
    pub help: Option<String>,
}

impl From<&str> for ErrorInfo {
    fn from(name: &str) -> Self {
        Self {
            message: var(name, "message").unwrap(),
            code: var(name, "code").unwrap().parse::<i32>().unwrap(),
            help: var(name, "help"),
        }
    }
}

#[inline]
fn var(name: &str, postfix: &str) -> Option<String> {
    match std::env::var(format!("{}.{}", name, postfix)) {
        Ok(value) => Some(value),
        Err(_) => None,
    }
}

lazy_static! {
    pub static ref EXPECTED_NEXT_TOKEN: ErrorInfo = ErrorInfo::from("ExpectedNextToken");
    pub static ref EXPECTED_DATA_TYPE: ErrorInfo = ErrorInfo::from("ExpectedDataType");
    pub static ref EXPECTED_EXPRESSION: ErrorInfo = ErrorInfo::from("ExpectedExpression");
    pub static ref UNEXPECTED_TOKEN: ErrorInfo = ErrorInfo::from("UnexpectedToken");
    pub static ref TYPE_MISMATCH: ErrorInfo = ErrorInfo::from("TypeMismatch");
    pub static ref IDENTIFIER_NOT_DEFINED: ErrorInfo = ErrorInfo::from("IdentifierNotDefined");
    pub static ref TYPE_ALIAS_NOT_DEFINED: ErrorInfo = ErrorInfo::from("TypeAliasNotDefined");
    pub static ref IDENTIFIER_ALREADY_DEFINED: ErrorInfo =
        ErrorInfo::from("IdentifierAlreadyDefined");
    pub static ref TYPE_ALIAS_ALREADY_DEFINED: ErrorInfo =
        ErrorInfo::from("TypeAliasAlreadyDefined");
    pub static ref OPERATOR_NOT_SUPPORTED: ErrorInfo = ErrorInfo::from("OperatorNotSupported");
    pub static ref TYPE_ANNOTATION_NEEDED: ErrorInfo = ErrorInfo::from("TypeAnnotationNeeded");
    pub static ref CANNOT_ASSIGN_TO_IMMUTABLE_VARIABLE: ErrorInfo =
        ErrorInfo::from("CannotAssignToImmutableVariable");
    pub static ref INVALID_OPERATOR1: ErrorInfo = ErrorInfo::from("InvalidOperator1");
    pub static ref INVALID_OPERATOR2: ErrorInfo = ErrorInfo::from("InvalidOperator2");
}

fn replace(error_info: ErrorInfo, replacements: &[&str]) -> ErrorInfo {
    let mut text = error_info.message;
    for (index, replacement) in replacements.iter().enumerate() {
        text = text.replace(&format!("{{{}}}", index), replacement);
    }

    ErrorInfo {
        message: text,
        code: error_info.code,
        help: error_info.help,
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub error: ErrorInfo,
    pub position: Position,
}

impl From<ParsingError> for Error {
    fn from(error: ParsingError) -> Self {
        use parser::ParsingErrorKind::*;

        let message = match error.message {
            ExpectedNextToken(a, b) => replace(EXPECTED_NEXT_TOKEN.clone(), &[&a, &b]),
            ExpectedDataType(a) => replace(EXPECTED_DATA_TYPE.clone(), &[&a]),
            ExpectedExpression(a) => replace(EXPECTED_EXPRESSION.clone(), &[&a]),
            UnexpectedToken(a) => replace(UNEXPECTED_TOKEN.clone(), &[&a]),
        };

        Self {
            error: message,
            position: error.position,
        }
    }
}

impl From<SemanticError> for Error {
    fn from(error: SemanticError) -> Self {
        use analyzer::SemanticErrorKind::*;

        let message = match error.message {
            TypeMismatch(a, b) => replace(TYPE_MISMATCH.clone(), &[&a, &b]),
            IdentifierNotDefined(a) => replace(IDENTIFIER_NOT_DEFINED.clone(), &[&a]),
            TypeAliasNotDefined(a) => replace(TYPE_ALIAS_NOT_DEFINED.clone(), &[&a]),
            IdentifierAlreadyDefined(a) => replace(IDENTIFIER_ALREADY_DEFINED.clone(), &[&a]),
            TypeAliasAlreadyDefined(a) => replace(TYPE_ALIAS_ALREADY_DEFINED.clone(), &[&a]),
            OperatorNotSupported(a, b) => replace(OPERATOR_NOT_SUPPORTED.clone(), &[&a, &b]),
            TypeAnnotationNeeded => TYPE_ANNOTATION_NEEDED.clone(),
            CannotAssignToImmutableVariable(a) => {
                replace(CANNOT_ASSIGN_TO_IMMUTABLE_VARIABLE.clone(), &[&a])
            }
        };

        Self {
            error: message,
            position: error.position,
        }
    }
}

impl From<EvaluateError> for Error {
    fn from(error: EvaluateError) -> Self {
        use evaluator::EvaluateErrorKind::*;

        let message = match error.message {
            IdentifierAlreadyDefined(a) => replace(IDENTIFIER_ALREADY_DEFINED.clone(), &[&a]),
            IdentifierNotDefined(a) => replace(IDENTIFIER_NOT_DEFINED.clone(), &[&a]),
            InvalidOperator1(a, b) => replace(INVALID_OPERATOR1.clone(), &[&a, &b]),
            InvalidOperator2(a, b, c) => replace(INVALID_OPERATOR2.clone(), &[&a, &b, &c]),
        };

        Self {
            error: message,
            position: error.position,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Error {
            error:
                ErrorInfo {
                    message,
                    code,
                    help,
                },
            position,
        } = self;
        let help = match help {
            Some(help) => format!("\nHelp: {}", help),
            None => String::new(),
        };

        write!(f, "Error[{code}] at {position}: {message}{help}",)
    }
}

fn main() {
    let args = arguments::Cli::parse();
    let content = fs::read_to_string(args.source).unwrap();

    let lexer = Lexer::new(content.as_str());
    let mut parser = Parser::new(lexer);

    match parser.parse_program() {
        Ok(ast) => {
            // println!("AST: {:#?}", ast);
            match Analyzer::new(ast.clone()).analyze() {
                Ok(ret) => {
                    println!("Analyzed return type: {ret:?}");
                    if args.eval {
                        if let Err(error) = Evaluator::new(ast).evaluate() {
                            println!("{}", Error::from(error));
                        }
                    }
                }
                Err(error) => println!("{}", Error::from(error)),
            }
        }
        Err(errors) => {
            for error in errors {
                println!("{}", Error::from(error));
            }
        }
    }
}
