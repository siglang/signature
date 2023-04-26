use crate::symbol_table::SymbolTable;
use parser::ast::Program;

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
        self.program.clone()
    }
}
