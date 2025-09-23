# From https://github.com/agu-z/roc-pg/blob/main/sql-cli/src/Schema.roc
# License is UPL
interface Schema
    exposes [
        Schema,
        ColumnId,
        Table,
        Column,
        new,
        getName,
        getTables,
        primaryColumn,
    ]
    imports []

Nullable a : [Null, NotNull a]

Schema := {
    name : Str,
    tables : List Table,
    references : Dict ColumnId ColumnId,
    primaryColumns : Dict ColumnId { tableName : Str, columnName : Str },
}

ColumnId : (I32, I16)

Table : {
    id : I32,
    name : Str,
    columns : List Column,
    constraints : List Constraint,
}

Column : {
    num : I16,
    name : Str,
    dataType : Str,
    typeCategory : Str,
    elemDataType : Nullable Str,
    isNullable : Bool,
}

Constraint : {
    type : Str,
    columns : List I16,
    foreignTable : I32, # pg sets this to 0 if not foreign key
    foreignColumns : List I16,
}

new : Str, List Table -> Schema
new = \schemaName, tables ->
    tablesLen : Nat
    tablesLen = List.len tables

    keys : {
        primaryColumns : Dict ColumnId { tableName : Str, columnName : Str },
        references : Dict ColumnId ColumnId,
    }
    keys =
        stateTable, table <- List.walk tables {
                primaryColumns: Dict.withCapacity tablesLen,
                references: Dict.withCapacity (tablesLen * 4),
            }
        state, constraint <- List.walk table.constraints stateTable

        when constraint.type is
            "p" ->
                newPrimaryColumns =
                    constraint.columns
                    |> List.map \colNum ->

                        names =
                            when List.findFirst table.columns \col -> col.num == colNum is
                                Ok { name } ->
                                    {
                                        tableName: table.name,
                                        columnName: name,
                                    }

                                Err NotFound ->
                                    {
                                        tableName: "\(Num.toStr table.id)",
                                        columnName: "\(Num.toStr colNum)",
                                    }

                        ((table.id, colNum), names)
                    |> Dict.fromList

                { state & primaryColumns: Dict.insertAll state.primaryColumns newPrimaryColumns }

            "f" ->
                newReferences =
                    constraint.columns
                    |> List.map2 constraint.foreignColumns \colNum, foreignColumn ->
                        ((table.id, colNum), (constraint.foreignTable, foreignColumn))
                    |> Dict.fromList

                { state & references: Dict.insertAll state.references newReferences }

            _ ->
                state

    @Schema {
        name: schemaName,
        tables,
        primaryColumns: keys.primaryColumns,
        references: keys.references,
    }

getTables : Schema -> List Table
getTables = \@Schema schema -> schema.tables

getName : Schema -> Str
getName = \@Schema schema -> schema.name

## Recursively find the final column referenced by another column.
##
## Returns itself if it's part of a primary key and no foreign key.
primaryColumn : Schema,
    ColumnId
    -> Result
        {
            id : ColumnId,
            tableName : Str,
            columnName : Str,
        }
        [KeyNotFound]
primaryColumn = \@Schema schema, column ->
    when Dict.get schema.references column is
        Ok refColumn ->
            primaryColumn (@Schema schema) refColumn

        Err KeyNotFound ->
            Dict.get schema.primaryColumns column
            |> Result.map \names -> {
                id: column,
                tableName: names.tableName,
                columnName: names.columnName,
            }

expect primaryColumn testSchema (1, 1) == Ok { id: (1, 1), tableName: "users", columnName: "id" }
expect primaryColumn testSchema (1, 2) == Err KeyNotFound
expect primaryColumn testSchema (2, 1) == Ok { id: (2, 1), tableName: "posts", columnName: "id" }
expect primaryColumn testSchema (2, 2) == Ok { id: (1, 1), tableName: "users", columnName: "id" }
expect primaryColumn testSchema (2, 3) == Err KeyNotFound

testSchema =
    new "public" [
        {
            id: 1,
            name: "users",
            columns: [
                {
                    num: 1,
                    name: "id",
                    dataType: "int4",
                    typeCategory: "N",
                    elemDataType: Null,
                    isNullable: Bool.false,
                },
                {
                    num: 2,
                    name: "name",
                    dataType: "text",
                    typeCategory: "S",
                    elemDataType: Null,
                    isNullable: Bool.false,
                },
            ],
            constraints: [
                {
                    type: "p",
                    columns: [1],
                    foreignTable: 0,
                    foreignColumns: [],
                },
            ],
        },
        {
            id: 2,
            name: "posts",
            columns: [
                {
                    num: 1,
                    name: "id",
                    dataType: "int4",
                    typeCategory: "N",
                    elemDataType: Null,
                    isNullable: Bool.false,
                },
                {
                    num: 2,
                    name: "user_id",
                    dataType: "int4",
                    typeCategory: "N",
                    elemDataType: Null,
                    isNullable: Bool.false,
                },
                {
                    num: 3,
                    name: "title",
                    dataType: "text",
                    typeCategory: "S",
                    elemDataType: Null,
                    isNullable: Bool.false,
                },
            ],
            constraints: [
                {
                    type: "p",
                    columns: [1],
                    foreignTable: 0,
                    foreignColumns: [],
                },
                {
                    type: "f",
                    columns: [2],
                    foreignTable: 1,
                    foreignColumns: [1],
                },
            ],
        },
    ]
