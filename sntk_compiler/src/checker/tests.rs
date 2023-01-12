#[cfg(test)]
mod tests {
    use crate::checker::checker::{Checker, CustomTypes, DeclaredTypes};
    use sntk_core::{
        parser::ast::{DataType, DataTypeKind, FunctionType, Position},
        tokenizer::token::TokenKind,
    };
    use sntk_ir::instruction::{Instruction, InstructionType, IrExpression, LiteralValue};
    use std::collections::HashMap;

    const POSITION: Position = Position(1, 0);

    #[test]
    fn identifier_type_test() {
        let declarations = DeclaredTypes {
            types: HashMap::from([("a".to_string(), DataType::new(DataTypeKind::Number, POSITION))]),
            parent: Some(Box::new(DeclaredTypes {
                types: HashMap::from([("a".to_string(), DataType::new(DataTypeKind::String, POSITION))]),
                parent: None,
            })),
        };

        assert_eq!(
            Checker::new(None, &declarations, &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Identifier("a".to_string()))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn block_return_type_test() {
        let declarations = DeclaredTypes {
            types: HashMap::from([
                ("a".to_string(), DataType::new(DataTypeKind::Number, POSITION)),
                ("b".to_string(), DataType::new(DataTypeKind::Number, POSITION)),
            ]),
            parent: None,
        };

        assert_eq!(
            Checker::new(None, &declarations, &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Block(vec![
                    Instruction::new(
                        InstructionType::StoreName("a".to_string(), IrExpression::Literal(LiteralValue::Number(5.))),
                        POSITION
                    ),
                    Instruction::new(
                        InstructionType::StoreName("b".to_string(), IrExpression::Literal(LiteralValue::Number(5.))),
                        POSITION
                    ),
                    Instruction::new(
                        InstructionType::Return(IrExpression::Infix(
                            Box::new(IrExpression::Identifier("a".to_string())),
                            TokenKind::Plus,
                            Box::new(IrExpression::Identifier("b".to_string()))
                        )),
                        POSITION
                    )
                ]))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn if_return_type_test() {
        let declarations = DeclaredTypes {
            types: HashMap::from([
                ("a".to_string(), DataType::new(DataTypeKind::Number, POSITION)),
                ("b".to_string(), DataType::new(DataTypeKind::Number, POSITION)),
            ]),
            parent: None,
        };

        let consequence = IrExpression::Block(vec![Instruction::new(
            InstructionType::Return(IrExpression::Literal(LiteralValue::Number(5.))),
            POSITION,
        )]);
        let alternative = IrExpression::Block(vec![Instruction::new(
            InstructionType::Return(IrExpression::Literal(LiteralValue::Number(10.))),
            POSITION,
        )]);

        assert_eq!(
            Checker::new(None, &declarations, &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::If(
                    Box::new(IrExpression::Literal(LiteralValue::Boolean(true))),
                    Box::new(consequence),
                    Box::new(Some(alternative))
                ))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn call_return_type_test() {
        let declarations = DeclaredTypes {
            types: HashMap::from([(
                "a".to_string(),
                DataType::new(
                    DataTypeKind::Fn(FunctionType {
                        generics: None,
                        parameters: vec![
                            (DataType::new(DataTypeKind::Number, POSITION), false),
                            (DataType::new(DataTypeKind::String, POSITION), true),
                        ],
                        return_type: Box::new(DataType::new(DataTypeKind::Number, POSITION)),
                    }),
                    POSITION,
                ),
            )]),
            parent: None,
        };

        assert_eq!(
            Checker::new(None, &declarations, &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Call(
                    Box::new(IrExpression::Identifier("a".to_string())),
                    vec![
                        IrExpression::Literal(LiteralValue::Number(5.)),
                        IrExpression::Literal(LiteralValue::Array(vec![
                            IrExpression::Literal(LiteralValue::String("foo".to_string())),
                            IrExpression::Literal(LiteralValue::String("bar".to_string())),
                            IrExpression::Literal(LiteralValue::String("baz".to_string())),
                        ]))
                    ]
                ))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn index_type() {
        assert_eq!(
            Checker::new(None, &DeclaredTypes::new(None), &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Index(
                    Box::new(IrExpression::Literal(LiteralValue::Array(vec![
                        IrExpression::Literal(LiteralValue::Number(5.)),
                        IrExpression::Literal(LiteralValue::Number(10.)),
                        IrExpression::Literal(LiteralValue::Number(15.)),
                    ]))),
                    Box::new(IrExpression::Literal(LiteralValue::Number(1.)))
                ))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn prefix_type() {
        assert_eq!(
            Checker::new(None, &DeclaredTypes::new(None), &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Prefix(
                    TokenKind::Minus,
                    Box::new(IrExpression::Literal(LiteralValue::Number(5.)))
                ))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn infix_type() {
        assert_eq!(
            Checker::new(None, &DeclaredTypes::new(None), &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Infix(
                    Box::new(IrExpression::Literal(LiteralValue::Number(5.))),
                    TokenKind::EQ,
                    Box::new(IrExpression::Literal(LiteralValue::Number(10.)))
                ))
                .unwrap(),
            DataType::new(DataTypeKind::Boolean, POSITION)
        );
    }

    #[test]
    fn literal_type_test() {
        assert_eq!(
            Checker::new(None, &DeclaredTypes::new(None), &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Literal(LiteralValue::Number(10.0)))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }
}