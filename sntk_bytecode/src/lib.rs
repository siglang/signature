/// **Built-in functions.**
pub mod builtin;
/// **Instruction commands, binary operations**
pub mod code;
/// **The error module contains the error handling for the bytecode runtime.**
pub mod error;
/// **Virtual machine interpreter.** it works on a stack basis.
pub mod interpreter;
/// **The stack on which the interpreter is based.**
pub mod stack;
