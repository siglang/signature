use std::*;

#[derive(Debug, PartialEq, Default, Clone)]
pub struct ByteCodeRuntime {
    pub message: String,
    pub pointer: usize,
}

impl ByteCodeRuntime {
    pub fn new(message: &str, args: Vec<String>, pointer: usize) -> Self {
        let mut message = message.to_string();

        args.iter().enumerate().for_each(|(i, arg)| {
            message = message.replace(&format!("{{{i}}}"), arg);
        });

        ByteCodeRuntime { message, pointer }
    }
}

impl fmt::Display for ByteCodeRuntime {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} (on {})", self.message, self.pointer)
    }
}

macro_rules! messages {
    ($( $name:ident => $message:expr );*;) => {
        $(
            pub const $name: &str = $message;
        )*
    };
}

messages! {
    UNKNOWN_FUNCTION => "Unknown function: {0}";
    NOT_A_FUNCTION => "Not a function: {0}";
    INVALID_OPERAND => "Invalid operand: {0}";
}

#[macro_export]
macro_rules! runtime_error {
    ($self:ident; $msg:ident; $( $r:expr ),*) => {
        panic!("{}", ByteCodeRuntime::new($msg, vec![$( $r.to_string() ),*], $self.instruction_pointer))
    };
}
