use std::{cell::*, rc::*};

/// Depending on this options, the behavior of the compiler is different.
///
/// the following describes the possible options and default values for compiler options:
///
/// * `eee_opt_level` = `0`, `1` or `2`, default `1`
#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub eee_opt_level: Rc<RefCell<u8>>,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            eee_opt_level: Rc::new(RefCell::new(1)),
        }
    }
}

macro_rules! impl_compiler_options {
    ($($name:ident, $set:ident => $t:ty);*) => {
        impl CompilerOptions {
            $(
                pub fn $name(&self) -> $t {
                    *self.$name.borrow()
                }

                pub fn $set(&self, value: $t) {
                    *self.$name.borrow_mut() = value;
                }
            )*
        }
    }
}

impl_compiler_options! {
    eee_opt_level, set_eee_opt_level => u8
}
