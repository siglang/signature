> **Note**
> 
> <ins>underline</ins> is a comment. 

# Syntax

### `fn`

* `fn`[^keyword] < [*`Generics`*](#generics)[^optional] > ( `ParameterName`[^ident]: `ParameterType`[^type] ) -> `ReturnType`[^type]
    * <ins>inline</ins> -> [`Expression`](#expression);
    * <ins>block</ins> { [`Statements`](#statement)[^repeat] }

### `generics`

* `GenericName`[^ident] < `GenericParameters`[^ident][^repeat] >

### `expression`

* `Expression`
    * ( [`Expression`](#expression) )
    * <ins>Infix Expression</ins> [`Expression`](#expression) [`InfixOperator`](#infixop)[^operator] [`Expression`](#expression)
    * <ins>Prefix Expression</ins> [`PrefixOperator`](#prefixop)[^operator] [`Expression`](#expression)
    * [`BlockExpression`](#block-expression)
    * [`Identifier`](#identifier)[^ident]
    * [`IfExpression`](#if)
    * [`CallExpression`](#call-expression)
    * [`IndexExpression`](#index-expression)
    * [`Literal`](#literal)

### `statement`

* `Statement`
    * [`ExpressionStatement`](#expression);
    * [`let`](#let) statement
    * [`type`](#type) statement

[^keyword]: [keyword](#keyword)
[^ident]: [identifier](#identifier)
[^type]: [type](#type)
[^literal]: [literal](#literal)
[^operator]: [operator](#op)
[^optional]: optional
[^repeat]: repeatable
