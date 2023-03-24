use crate::{
    types::{IrProgram, IrStatement},
    CompilingError, CompilingErrorKind,
};
use parser::parser::{Position, Program, Statement};

#[derive(Debug)]
pub struct Compiler(pub Program);

pub type CompileResult<T> = Result<T, Vec<CompilingError>>;

impl Compiler {
    pub fn compile_program(&mut self) -> CompileResult<IrProgram> {
        if !self.0.errors.is_empty() {
            return Err(self
                .0
                .errors
                .iter()
                .map(|error| {
                    CompilingError::new(
                        CompilingErrorKind::ParsingError(error.clone()),
                        Position::default(),
                    )
                })
                .collect());
        }

        let mut ir_program = IrProgram::default();

        for statement in self.0.statements.clone().iter() {
            ir_program.0.push(self.compile_statement(statement)?);
        }

        Ok(ir_program)
    }

    fn compile_statement(&mut self, statement: &Statement) -> CompileResult<IrStatement> {
        todo!()
    }
}
