use std::cell::Cell;

#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub eee_opt_level: Cell<u8>,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self { eee_opt_level: 2.into() }
    }
}

macro_rules! impl_compiler_options {
    ($($name:ident, $set:ident => $t:ty);*) => {
        impl CompilerOptions {
            $(
                pub fn $name(&self) -> $t {
                    self.$name.get()
                }

                pub fn $set(&self, value: $t) {
                    self.$name.set(value)
                }
            )*
        }
    }
}

impl_compiler_options! {
    eee_opt_level, set_eee_opt_level => u8
}
