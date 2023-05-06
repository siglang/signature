use parser::ast::DataType;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolEntry {
    pub data_type: DataType,
    pub attributes: SymbolAttributes,
    pub kind: SymbolKind,
}

impl SymbolEntry {
    pub fn new(data_type: DataType, attributes: SymbolAttributes, kind: SymbolKind) -> Self {
        Self {
            data_type,
            attributes,
            kind,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct SymbolAttributes {
    pub is_spread: Option<bool>,
}

impl SymbolAttributes {
    pub fn is_spread(mut self, is_spread: bool) -> Self {
        self.is_spread = Some(is_spread);
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Variable,
    Named,
    // Struct - todo
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    pub entries: HashMap<String, SymbolEntry>,
    pub parent: Option<Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new(parent: Option<SymbolTable>) -> Self {
        Self {
            entries: HashMap::new(),
            parent: parent.map(Box::new),
        }
    }

    pub fn insert(&mut self, name: &str, entry: SymbolEntry) -> Option<()> {
        if self.entries.contains_key(name) {
            return None;
        }

        self.entries.insert(name.to_string(), entry);
        Some(())
    }

    pub fn lookup(&self, name: &str) -> Option<&SymbolEntry> {
        self.entries
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.lookup(name)))
    }

    pub fn lookup_mut(&mut self, name: &str) -> Option<&mut SymbolEntry> {
        self.entries.get_mut(name).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.lookup_mut(name))
        })
    }

    pub fn variable(&self, name: &str) -> Option<&SymbolEntry> {
        self.lookup(name)
            .filter(|entry| entry.kind == SymbolKind::Variable)
    }

    pub fn named(&self, name: &str) -> Option<&SymbolEntry> {
        self.lookup(name)
            .filter(|entry| entry.kind == SymbolKind::Named)
    }
}

/// for testing purposes
#[macro_export]
macro_rules! symbol_entry {
    ($data_type:ident, $kind:ident) => {
        $crate::symbol_table::SymbolEntry::new(
            DataType::new(DataTypeKind::$data_type, Position::default()),
            $crate::symbol_table::SymbolAttributes::default(),
            $crate::symbol_table::SymbolKind::$kind,
        )
    };
}

/// for testing purposes
#[macro_export]
macro_rules! symbol_table {
    (@option $parent:expr; $( $name:ident => $kind:ident, $data_type:ident; )*) => {
        {
            let mut symbol_table = SymbolTable::new($parent);

            $(
                symbol_table.insert(
                    stringify!($name),
                    $crate::symbol_entry!($data_type, $kind),
                );
            )*

            symbol_table
        }
    };
    ($( $name:ident => $kind:ident, $data_type:ident; )*) => {
        $crate::symbol_table! {
            @option
            None;
            $( $name => $kind, $data_type; )*
        }
    };
    ($parent:expr; $( $name:ident => $kind:ident, $data_type:ident; )*) => {
        $crate::symbol_table! {
            @option
            Some($parent);
            $( $name => $kind, $data_type; )*
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::ast::{DataTypeKind, Position};

    #[test]
    fn test_insert() {
        let mut symbol_table = SymbolTable::new(None);

        let entry = symbol_entry!(Number, Variable);
        symbol_table.insert("x", entry.clone()).unwrap();

        assert_eq!(symbol_table.entries.get("x"), Some(&entry));
    }

    #[test]
    fn test_insert_2() {
        let mut symbol_table = SymbolTable::new(None);

        symbol_table
            .insert("x", symbol_entry!(Number, Variable))
            .unwrap();

        assert_eq!(
            symbol_table.insert("x", symbol_entry!(Number, Variable)),
            None
        );
    }

    #[test]
    fn test_lookup() {
        let mut parent = SymbolTable::new(None);
        parent.insert("x", symbol_entry!(Number, Variable)).unwrap();

        let mut symbol_table = SymbolTable::new(Some(parent));
        symbol_table
            .insert("y", symbol_entry!(Number, Variable))
            .unwrap();

        let entry = symbol_entry!(String, Variable);
        symbol_table.insert("x", entry.clone()).unwrap();

        assert_eq!(symbol_table.lookup("x"), Some(&entry));
    }

    #[test]
    fn test_variable() {
        let mut symbol_table = SymbolTable::new(None);
        symbol_table
            .insert("x", symbol_entry!(Number, Named))
            .unwrap();

        assert_eq!(symbol_table.variable("x"), None);
    }

    #[test]
    fn test_variable_2() {
        let mut symbol_table = SymbolTable::new(None);

        let entry = symbol_entry!(Number, Variable);
        symbol_table.insert("x", entry.clone()).unwrap();

        assert_eq!(symbol_table.variable("x"), Some(&entry));
    }

    #[test]
    fn test_named() {
        let mut symbol_table = SymbolTable::new(None);
        symbol_table
            .insert("x", symbol_entry!(Number, Variable))
            .unwrap();

        assert_eq!(symbol_table.named("x"), None);
    }

    #[test]
    fn test_named_2() {
        let mut symbol_table = SymbolTable::new(None);

        let entry = symbol_entry!(Number, Named);
        symbol_table.insert("x", entry.clone()).unwrap();

        assert_eq!(symbol_table.named("x"), Some(&entry));
    }

    #[test]
    fn test_symbol_entry_macro() {
        let entry = symbol_entry!(Number, Variable);

        assert_eq!(
            entry,
            SymbolEntry::new(
                DataType::new(DataTypeKind::Number, Position::default()),
                SymbolAttributes::default(),
                SymbolKind::Variable,
            )
        );
    }

    #[test]
    fn test_symbol_table_macro() {
        let symbol_table = symbol_table! {
            symbol_table! {
                x => Variable, Number;
            };
            y => Named, String;
            z => Variable, Boolean;
        };

        assert_eq!(
            symbol_table,
            SymbolTable {
                entries: {
                    let mut entries = HashMap::new();
                    entries.insert("y".to_string(), symbol_entry!(String, Named));
                    entries.insert("z".to_string(), symbol_entry!(Boolean, Variable));
                    entries
                },
                parent: Some(Box::new(SymbolTable {
                    entries: {
                        let mut entries = HashMap::new();
                        entries.insert("x".to_string(), symbol_entry!(Number, Variable));
                        entries
                    },
                    parent: None,
                })),
            }
        );
    }
}
