use parser::ast::DataType;

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTableEntry {
    pub name: String,
    pub data_type: DataType,
    pub scope: String,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct SymbolTable {
    pub entries: Vec<SymbolTableEntry>,
}
