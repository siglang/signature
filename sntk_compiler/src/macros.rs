#[macro_export]
macro_rules! type_error {
    ($msg:ident; $( $r:expr ),*; $pos:expr;) => {
        CompileError::TypeError(TypeError::new($msg, vec![$( format!("{}", $r) ),*], $pos))
    };
}
