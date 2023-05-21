# Syntax

## `statement`

-   `Statement`
    -   [`ExpressionStatement`](#expression)`;` | [`LetStatement`](#letstatement) | [`MutStatement`](#mutstatement) | [`TypeStatement`](#typestatement) | [`DeclareStatement`](#declarestatement) | [`StructStatement`](#structtype) | [`ReturnStatement`](#returnstatement) | [`ReturnExpressionStatement`](#returnexpressionstatement)

<br />

### `LetStatement`

-   `LetStatement`
    -   `let`[^keyword] [`Identifier`](#identifier)`:` [`Type`](#types)[^type][^optional] `=` [`Expression`](#expression)`;`

<br />

### `MutStatement`

-   `MutStatement`
    -   `mut`[^keyword] [`Identifier`](#identifier)`:` [`Type`](#types)[^type][^optional] `=` [`Expression`](#expression)`;`

<br />

### `TypeStatement`

-   `TypeStatement`
    -   `type`[^keyword] [`Identifier`](#identifier) [_`Generics`_](#generics)[^optional] `=` [`Type`](#types)[^type]`;`

<br />

### `DeclareStatement`

-   `DeclareStatement`
    -   `declare`[^keyword] [`Identifier`](#identifier) `=` [`Type`](#types)[^type]`;`

<br />

### `ReturnStatement`

-   `ReturnStatement`
    -   `return`[^keyword] [`Expression`](#expression)`;`

<br />

### `ReturnExpressionStatement`

-   `ReturnExpressionStatement`
    -   [`Expression`](#expression)

---

## `expression`

-   `Expression`
    -   ( [`Expression`](#expression) )
    -   [`InfixOperator`](#infixoperator)
    -   [`PrefixOperator`](#prefixoperator)
    -   [`BlockExpression`](#blockexpression)
    -   [`Identifier`](#identifier)[^ident]
    -   [`IfExpression`](#ifexpression)
    -   [`CallExpression`](#callexpression)
    -   [`IndexExpression`](#indexexpression)
    -   [`Literal`](#literal)

<br />

### `InfixOperator`

-   [`Expression`](#expression) [`InfixOperator`](#infixoperator)[^operator] [`Expression`](#expression)
    -   `+` | `-` | `*` | `/` | `%` | `==` | `!=` | `>` | `<` | `>=` | `<=`

<br />

### `PrefixOperator`

-   [`PrefixOperator`](#prefixoperator)[^operator] [`Expression`](#expression)
    -   `!` | `-`

<br />

### `BlockExpression`

-   `BlockExpression`
    -   `{` [`Statement`](#statement) `}`

<br />

### `Identifier`

-   `Identifier`
    -   `a`-`z` `A`-`Z` `0`-`9` `_`

<br />

### `IfExpression`

-   `IfExpression`
    -   `if`[^keyword] [`Expression`](#expression) [`BlockExpression`](#blockexpression) [_`ElseClause`_](#elseclause)[^optional]

<br />

#### `ElseClause`

-   `ElseClause`
    -   `else`[^keyword] [`BlockExpression`](#blockexpression)
    -   `else`[^keyword] [`IfExpression`](#ifexpression)

<br />

### `CallExpression`

-   `CallExpression`
    -   [`Expression`](#expression) `(` [`Arguments`](#expression)[^repeat] `)`

<br />

### `IndexExpression`

-   `IndexExpression`
    -   [`Expression`](#expression) `[` [`Expression`](#expression) `]`

<br />

### `TypeofExpression`

-   `TypeofExpression`
    -   `typeof`[^keyword] [`Expression`](#expression)

<br />

### `Literal`

-   `Literal`
    -   [`NumberLiteral`](#numberliteral) | [`StringLiteral`](#stringliteral) | [`BooleanLiteral`](#booleanliteral) | [`ArrayLiteral`](#arrayliteral) | [`FunctionLiteral`](#functionliteral) | [`StructLiteral`](#structliteral)

<br />

#### `NumberLiteral`

-   `NumberLiteral`
    -   `0`-`9` `.`

<br />

#### `StringLiteral`

-   `StringLiteral`
    -   `"` `string` `"`

### `BooleanLiteral`

-   `BooleanLiteral`
    -   `true` | `false`

<br />

#### `ArrayLiteral`

-   `ArrayLiteral`
    -   `[` [`Expression`](#expression)[^repeat] `]`

<br />

#### `FunctionLiteral`

-   `fn`[^keyword] [_`Generics`_](#generics)[^optional] `(` [`FunctionParameters`](#functionparameters)[^repeat] `)` `->` `ReturnType`[^type] [`FunctionBlock`](#functionblock)

<br />

##### `FunctionParameters`

-   `FunctionParameter`
    -   `ParameterName`[^ident]`:` `ParameterType`[^type]

<br />

##### `FunctionBlock`

-   `FunctionBlock`
    -   `->` [`Expression`](#expression)`;`
    -   `{` [`Statements`](#statement)[^repeat] `}`

<br />

#### `StructLiteral`

-   `StructLiteral`
    -   `struct`[^keyword] `StructName`[^ident] `{` [`StructLiteralFields`](#structliteralfields)[^repeat] `}`

<br />

##### `StructLiteralFields`

-   `StructLiteralField`
    -   `FieldName`[^ident]`:` [`FieldValue`](#expression)

---

## `Types`

### `Primitive Types`

-   `PrimitiveType`
    -   `number` | `string` | `boolean` | `void`

<br />

### `Sequence Types`

-   `SequenceType`
    -   [`ArrayType`](#arraytype) | [`FunctionType`](#functiontype)

#### `ArrayType`

-   `ArrayType`
    -   [`PrimitiveType`](#primitive-types)`[` `]`

<br />

#### `FunctionType`

-   `FunctionType`
    -   `fn` [^keyword] `(` `ParameterType`[^type][^repeat] `)` `->` `ReturnType`[^type]

<br />

### `User Defined Types`

-   `UserDefinedType`
    -   [`StructType`](#structtype-structstatement)

#### `StructType` (`StructStatement`)

-   `StructType` (`StructStatement`)
    -   `struct` `Identifier`[^ident] [_`Generics`_](#generics)[^optional] `{` [`StructFields`](#structfield)[^repeat] `}`

<br />

##### `StructFields`

-   `StructField`
    -   `FieldName`[^ident]`:` `FieldType`[^type]

<br />

### `Generics`

-   `GenericName`[^ident] `<` `GenericParameters`[^ident][^repeat] `>`

---

## `Comments`

### `Single Line Comments`

-   `//` `Comment`

<br />

### `Multi Line Comments`

-   `/*` `Comment` `*/`

---

## `Priorities`

| `n` | Priority      | Operators                 |
| --- | ------------- | ------------------------- |
| `1` | `Lowest`      |                           |
| `2` | `Dot`         | `.` `->`                  |
| `3` | `Equals`      | `=` `==` `!=`             |
| `4` | `LessGreater` | `<` `>` `<=` `>=`         |
| `5` | `Sum`         | `+` `-`                   |
| `6` | `Product`     | `*` `/`                   |
| `7` | `Prefix`      | `!` `-`                   |
| `8` | `Call`        | [`( )`](#callexpression)  |
| `9` | `Index`       | [`[ ]`](#indexexpression) |

[^ident]: [identifier](#identifier)
[^type]: [type](#types)
[^literal]: [literal](#literal)
[^repeat]: repeatable (ends with comma (`,`)) (can be empty)
[^keyword]: keyword
[^operator]: operator
[^optional]: optional
