/// **Instruction commands, binary operations**
pub mod code;
/// The error module contains the error handling for the bytecode runtime.
pub mod error;
/// **Virtual machine interpreter.** it works on a stack basis.
///
/// for example:
/// ```rust
/// use sntk_bytecode::{code::*, interpreter::*, stack::*};
///
/// Interpreter::new(
///     vec![
///         /* Line 1 */ Instruction::LoadConst(0), // 5
///         /* Line 1 */ Instruction::LoadConst(1), // 2
///         /* Line 1 */ Instruction::BinaryOp(BinaryOp::Add), // 5 + 2
///         /* Line 1 */ Instruction::StoreName(0), // a = 7
///         /* Line 2 */ Instruction::LoadName(0), // a, 7
///         /* Line 2 */ Instruction::LoadGlobal(0), // print
///         /* Line 2 */ Instruction::CallFunction(1), // print(7)
///         /* Line 3 */ Instruction::LoadName(0), // a, 7
///         /* Line 3 */ Instruction::LoadConst(2), // 10
///         /* Line 3 */ Instruction::BinaryOp(BinaryOp::Mul), // 7 * 10
///         /* Line 3 */ Instruction::StoreName(1), // b = 70
///         /* Line 4 */ Instruction::LoadName(1), // b, 70
///         /* Line 4 */ Instruction::LoadName(0), // a, 7
///         /* Line 4 */ Instruction::BinaryOp(BinaryOp::Add), // 70 + 7
///         /* Line 4 */ Instruction::LoadGlobal(0), // print
///         /* Line 4 */ Instruction::CallFunction(1), // print(77)
///     ],
///     vec![
///         Value::LiteralValue(LiteralValue::Number(5.0)),
///         Value::LiteralValue(LiteralValue::Number(2.0)),
///         Value::LiteralValue(LiteralValue::Number(10.0)),
///     ],
///     vec!["print".to_string(), "a".to_string(), "b".to_string()],
/// )
/// .run();
/// ```
pub mod interpreter;
/// **Stack implementation.**
///
/// The stack on which the interpreter is based.
pub mod stack;
