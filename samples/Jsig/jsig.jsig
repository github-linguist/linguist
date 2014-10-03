type GenericE : {
    type: "genericLiteral",
    value: TypeExpression,
    generics: Array<TypeExpression>,
    label: String | null,
    optional: Boolean
}

type FunctionE : {
    type: "function",
    args: Array<TypeExpression>,
    result: TypeExpression,
    thisArg: TypeExpression | null,
    label: String | null,
    optional: Boolean
}

type ValueE : {
    type: "valueLiteral",
    value: String,
    name: String,
    label: String | null,
    optional: Boolean
}

type LiteralE : {
    type: "typeLiteral",
    name: String,
    builtin: Boolean,
    label: String | null,
    optional: Boolean
}

type UnionE : {
    type: "unionType",
    unions: Array<TypeExpression>,
    label: String | null,
    optional: Boolean
}

type IntersectionE : {
    type: "intersectionType",
    intersections: Array<TypeExpression>,
    label: String | null,
    optional: Boolean
}

type KeyValue : {
    type: "keyValue",
    key: String,
    value: TypeExpression,
    optional: Boolean
}

type ObjectE : {
    type: "object",
    keyValues: Array<KeyValue>,
    label: String | null,
    optional: Boolean
}

type TupleE : {
    type: "tuple",
    values: Array<TypeExpression>,
    label: String | null,
    optional: Boolean
}

type TypeExpression : ObjectE | UnionE | LiteralE | FunctionE |
    ValueE | GenericE | TupleE | IntersectionE

type Assignment : {
    type: "assignment",
    identifier: String,
    typeExpression: TypeExpression
}

type TypeDeclaration : {
    type: "typeDeclaration",
    identifier: String,
    typeExpression: TypeExpression,
    generics: Array<LiteralE>
}

type Import : {
    type: "import",
    dependency: String,
    types: Array<LiteralE>
}

type Statement : Import | TypeDeclaration | Assignment

type Program : {
    type: "program",
    statements: Array<Statement>
}

type AST : {
    program: (Array<Statement>) => Program,
    typeDeclaration: (String, TypeExpression) => TypeDeclaration,
    assignment: (String, TypeExpression) => Assignment,
    importStatement: (String, Array<LiteralE>) => Import,
    object: (
        keyValues: Array<KeyValue> | Object<String, TypeExpression>,
        label?: String
    ) => ObjectE,
    union: (Array<TypeExpression>, label?: String, opts?: {
        optional: Boolean
    }) => UnionE,
    intersection: (Array<TypeExpression>, label?: String, opts?: {
        optional: Boolean
    }) => IntersectionE,
    literal: (String, builtin?: String, opts?: {
        optional: Boolean
    }) => LiteralE,
    keyValue: (String, TypeExpression) => KeyValue,
    value: (String, name: String, label?: String) => ValueE,
    functionType: (opts: {
        args?: Array<TypeExpression>,
        result: TypeExpression,
        thisArg?: TypeExpression,
        label?: String,
        optional?: Boolean
    }) => FunctionE,
    generic: (
        value: TypeExpression,
        generics: Array<TypeExpression>,
        label?: String
    ) => GenericE,
    tuple: (Array<TypeExpression>, label?: String, opts?: {
        optional: Boolean
    }) => TupleE
}

jsig/ast : AST

jsig/parser : (content: String) => Program
