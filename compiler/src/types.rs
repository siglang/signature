use crate::CompilingError;
use parser::{
    parser::{ParameterKind, Position},
    tokenizer::TokenKind,
};

#[derive(Debug, PartialEq, Clone)]
pub struct IrProgram<'a> {
    pub statements: Vec<IrStatement<'a>>,
    pub errors: Vec<CompilingError>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IrStatement<'a> {
    LetStatement(IrLetStatement<'a>),
    ReturnStatement(IrReturnStatement<'a>),
    IrExpressionStatement(IrExpressionStatement<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IrExpression<'a> {
    Identifier(IrIdentifier<'a>),
    BlockExpression(IrBlockExpression<'a>),
    PrefixExpression(IrPrefixExpression<'a>),
    InfixExpression(IrInfixExpression<'a>),
    IfExpression(IrIfExpression<'a>),
    FunctionLiteral(IrFunctionLiteral<'a>),
    CallExpression(IrCallExpression<'a>),
    TypeofExpression(IrTypeofExpression<'a>),
    IndexExpression(IrIndexExpression<'a>),
    StringLiteral(IrStringLiteral<'a>),
    NumberLiteral(IrNumberLiteral),
    ArrayLiteral(IrArrayLiteral<'a>),
    BooleanLiteral(IrBooleanLiteral),
    StructLiteral(IrStructLiteral<'a>),
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
                $identifier { $($field,)* position: position.clone() }
            }
        }
    };
    (<$($lt:tt),*> $identifier:ident => $( $field:ident: $type:ty ),*) => {
        #[derive(Debug, PartialEq, Clone)]
        pub struct $identifier<$($lt),*> {
            $( pub $field: $type, )*
            pub position: Position
        }

        impl<$($lt,)*> $identifier<$($lt,)*> {
            #[inline]
            pub fn new($( $field: $type, )* position: Position) -> Self {
                $identifier { $($field,)* position: position.clone() }
            }
        }
    };
}

make_struct! { <'a> IrLetStatement => identifier: IrIdentifier<'a>, value: IrExpression<'a> }
make_struct! { <'a> IrReturnStatement => value: IrExpression<'a> }
make_struct! { <'a> IrExpressionStatement => expression: IrExpression<'a> }

make_struct! { <'a> IrBlockExpression => statements: Vec<IrStatement<'a>> }
make_struct! { <'a> IrIfExpression => condition: Box<IrExpression<'a>>, consequence: Box<IrBlockExpression<'a>>, alternative: Option<Box<IrBlockExpression<'a>>> }
make_struct! { <'a> IrCallExpression => function: Box<IrExpression<'a>>, arguments: Vec<IrExpression<'a>> }
make_struct! { <'a> IrTypeofExpression => expression: Box<IrExpression<'a>> }
make_struct! { <'a> IrIndexExpression => left: Box<IrExpression<'a>>, index: Box<IrExpression<'a>> }
make_struct! { <'a> IrPrefixExpression => operator: TokenKind, right: Box<IrExpression<'a>> }
make_struct! { <'a> IrInfixExpression => left: Box<IrExpression<'a>>, operator: TokenKind, right: Box<IrExpression<'a>> }

make_struct! { <'a> IrIdentifier => value: &'a str }
make_struct! { <'a> IrStringLiteral => value: &'a str }
make_struct! { IrNumberLiteral => value: f64 }
make_struct! { IrBooleanLiteral => value: bool }
make_struct! { <'a> IrFunctionLiteral => parameters: Vec<Parameter<'a>>, body: IrBlockExpression<'a> }
make_struct! { <'a> IrArrayLiteral => elements: Vec<IrExpression<'a>> }
make_struct! { <'a> IrStructLiteral => identifier: IrIdentifier<'a>, fields: Vec<(IrIdentifier<'a>, IrExpression<'a>)> }

make_struct! { <'a> Parameter => identifier: IrIdentifier<'a>, kind: ParameterKind }
