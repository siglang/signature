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
///         /* Line 2 */ Instruction::LoadGlobal(1), // print
///         /* Line 2 */ Instruction::CallFunction(1), // print(7)
///         /* Line 3 */ Instruction::LoadName(0), // a, 7
///         /* Line 3 */ Instruction::LoadConst(2), // 10
///         /* Line 3 */ Instruction::LoadConst(3), // 20
///         /* Line 3 */ Instruction::LoadConst(4), // "hello"
///         /* Line 3 */ Instruction::LoadConst(5), // "world"
///         /* Line 3 */ Instruction::BinaryOp(BinaryOp::Add), // "hello" + "world"
///         /* Line 3 */ Instruction::LoadGlobal(2), // foo
///         /* Line 3 */ Instruction::CallFunction(4), // foo(7, 10, 20, "helloworld")
///         /* Line 4 */ Instruction::LoadName(0), // a, 7
///         /* Line 4 */ Instruction::LoadConst(1), // 2
///         /* Line 4 */ Instruction::BinaryOp(BinaryOp::Add), // 7 + 2
///         /* Line 4 */ Instruction::LoadConst(6), // 2
///         /* Line 4 */ Instruction::BinaryOp(BinaryOp::Mul), // (7 + 2) * 2
///         /* Line 4 */ Instruction::StoreName(0), // a = 16
///         /* Line 5 */ Instruction::LoadName(0), // a, 16
///         /* Line 5 */ Instruction::LoadGlobal(1), // print
///         /* Line 5 */ Instruction::CallFunction(1), // print(16)
///     ],
///     vec![
///         Value::LiteralValue(LiteralValue::Number(5.0)),
///         Value::LiteralValue(LiteralValue::Number(2.0)),
///         Value::LiteralValue(LiteralValue::Number(10.0)),
///         Value::LiteralValue(LiteralValue::Number(20.0)),
///         Value::LiteralValue(LiteralValue::String("hello".to_string())),
///         Value::LiteralValue(LiteralValue::String("world".to_string())),
///         Value::LiteralValue(LiteralValue::Number(2.0)),
///     ],
///     vec![
///         "a".to_string(),
///         "print".to_string(),
///         "foo".to_string(),
///     ],
/// ).run()
/// ```
pub mod interpreter;
/// **Stack implementation.**
///
/// The stack on which the interpreter is based.
pub mod stack;
