use parser::{
    parser::{BooleanLiteral, Identifier, NumberLiteral, ParameterKind, Position, StringLiteral},
    tokenizer::TokenKind,
};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct IrProgram(pub Vec<IrStatement>);

#[derive(Debug, PartialEq, Clone)]
pub enum IrStatement {
    LetStatement(IrLetStatement),
    ReturnStatement(IrReturnStatement),
    IrExpressionStatement(IrExpressionStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IrExpression {
    Identifier(IrIdentifier),
    BlockExpression(IrBlockExpression),
    PrefixExpression(IrPrefixExpression),
    InfixExpression(IrInfixExpression),
    IfExpression(IrIfExpression),
    FunctionLiteral(IrFunctionLiteral),
    CallExpression(IrCallExpression),
    TypeofExpression(IrTypeofExpression),
    IndexExpression(IrIndexExpression),
    StringLiteral(IrStringLiteral),
    NumberLiteral(IrNumberLiteral),
    ArrayLiteral(IrArrayLiteral),
    BooleanLiteral(IrBooleanLiteral),
    StructLiteral(IrStructLiteral),
}

macro_rules! make_struct {
    ($identifier:ident => $( $field:ident: $type:ty ),*) => {
        #[derive(Debug, PartialEq, Clone)]
        pub struct $identifier {
            $( pub $field: $type, )*
            pub position: Position
        }

        impl $identifier {
            #[inline]
            pub fn new($( $field: $type, )* position: Position) -> Self {
                $identifier {$($field,)* position: position.clone() }
            }
        }
    };
}

make_struct! { IrLetStatement => identifier: IrIdentifier, value: IrExpression }
make_struct! { IrReturnStatement => value: IrExpression }
make_struct! { IrExpressionStatement => expression: IrExpression }

make_struct! { IrBlockExpression => statements: Vec<IrStatement> }
make_struct! { IrIfExpression => condition: Box<IrExpression>, consequence: Box<IrBlockExpression>, alternative: Option<Box<IrBlockExpression>> }
make_struct! { IrCallExpression => function: Box<IrExpression>, arguments: Vec<IrExpression> }
make_struct! { IrTypeofExpression => expression: Box<IrExpression> }
make_struct! { IrIndexExpression => left: Box<IrExpression>, index: Box<IrExpression> }
make_struct! { IrPrefixExpression => operator: TokenKind, right: Box<IrExpression> }
make_struct! { IrInfixExpression => left: Box<IrExpression>, operator: TokenKind, right: Box<IrExpression> }

make_struct! { IrIdentifier => value: String }
make_struct! { IrStringLiteral => value: String }
make_struct! { IrNumberLiteral => value: f64 }
make_struct! { IrBooleanLiteral => value: bool }
make_struct! { IrFunctionLiteral => parameters: Vec<Parameter>, body: IrBlockExpression }
make_struct! { IrArrayLiteral => elements: Vec<IrExpression> }
make_struct! { IrStructLiteral => identifier: IrIdentifier, fields: Vec<(IrIdentifier, IrExpression)> }

make_struct! { Parameter => identifier: IrIdentifier, kind: ParameterKind }

impl From<Identifier> for IrIdentifier {
    fn from(identifier: Identifier) -> Self {
        IrIdentifier::new(identifier.value, identifier.position)
    }
}

impl From<NumberLiteral> for IrNumberLiteral {
    fn from(number: NumberLiteral) -> Self {
        IrNumberLiteral::new(number.value, number.position)
    }
}

impl From<StringLiteral> for IrStringLiteral {
    fn from(string: StringLiteral) -> Self {
        IrStringLiteral::new(string.value, string.position)
    }
}

impl From<BooleanLiteral> for IrBooleanLiteral {
    fn from(boolean: BooleanLiteral) -> Self {
        IrBooleanLiteral::new(boolean.value, boolean.position)
    }
}
