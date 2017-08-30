section RedshiftODBC;

// This is a work around for the lack of available conversion functions in the driver.
ImplicitTypeConversions = #table(
    { "Type1",        "Type2",         "ResultType" }, {
    // 'bpchar' char is added here to allow it to be converted to 'char' when compared against constants.
    { "bpchar",       "char",          "char" }
});

// This table below is required to work around columns with type name 'bpchar' not listed by SQLGetTypeInfo.
// It is a copy of what is returned by the driver with the addition of an entry for 'bpchar'.
//
// On a related note: The driver also returns columns with data type name 'bool' which are not listed in this table.
// While it is possible to add this type to this table and to the implicit conversion table above, the driver will 
// return incorrect results when comparison the column against other columns or when trying to project the column 
// to other types. 
SQLGetTypeInfo = #table(
    { "TYPE_NAME",      "DATA_TYPE", "COLUMN_SIZE", "LITERAL_PREF", "LITERAL_SUFFIX", "CREATE_PARAS",           "NULLABLE", "CASE_SENSITIVE", "SEARCHABLE", "UNSIGNED_ATTRIBUTE", "FIXED_PREC_SCALE", "AUTO_UNIQUE_VALUE", "LOCAL_TYPE_NAME", "MINIMUM_SCALE", "MAXIMUM_SCALE", "SQL_DATA_TYPE", "SQL_DATETIME_SUB", "NUM_PREC_RADIX", "INTERNAL_PRECISION", "USER_DATA_TYPE" }, {

    // Already returned by driver:
    { "char",           1,          65535,          "'",            "'",              "max. length",            1,          1,                3,            null,                 0,                  null,                "char",            null,            null,            -8,              null,               null,             0,                    0                }, 
    { "int8",           -5,         19,             "'",            "'",              null,                     1,          0,                2,            0,                    10,                 0,                   "int8",            0,               0,               -5,              null,               2,                0,                    0                },
    { "bit",            -7,         1,              "'",            "'",              null,                     1,          1,                3,            null,                 0,                  null,                "bit",             null,            null,            -7,              null,               null,             0,                    0                },
    { "bool",           -7,         1,              "'",            "'",              null,                     1,          1,                3,            null,                 0,                  null,                "bit",             null,            null,            -7,              null,               null,             0,                    0                },
    { "date",           9,          10,             "'",            "'",              null,                     1,          0,                2,            null,                 0,                  null,                "date",            null,            null,            9,               1,                  null,             0,                    0                }, 
    { "numeric",        3,          28,             null,           null,             null,                     1,          0,                2,            0,                    0,                   0,                  "numeric",         0,               0,               2,               null,               10,               0,                    0                },
    { "float8",         8,          15,             null,           null,             null,                     1,          0,                2,            0,                    0,                   0,                  "float8",          null,            null,            6,               null,               2,                0,                    0                },
    { "float8",         6,          17,             null,           null,             null,                     1,          0,                2,            0,                    0,                   0,                  "float8",          null,            null,            6,               null,               2,                0,                    0                },
    { "uuid",           -11,        37,             null,           null,             null,                     1,          0,                2,            null,                 0,                  null,                "uuid",            null,            null,            -11,             null,               null,             0,                    0                },
    { "int4",           4,          10,             null,           null,             null,                     1,          0,                2,            0,                    0,                   0,                  "int4",            0,               0,               4,               null,               2,                0,                    0                },
    { "text",           -1,         65535,          "'",            "'",              null,                     1,          1,                3,            null,                 0,                  null,                "text",            null,            null,            -10,             null,               null,             0,                    0                },
    { "lo",             -4,         255,            "'",            "'",              null,                     1,          0,                2,            null,                 0,                  null,                "lo",              null,            null,            -4,              null,               null,             0,                    0                }, 
    { "numeric",        2,          28,             null,           null,             "precision, scale",       1,          0,                2,            0,                    10,                 0,                   "numeric",         0,               6,               2,               null,               10,               0,                    0                },
    { "float4",         7,          9,              null,           null,             null,                     1,          0,                2,            0,                    10,                 0,                   "float4",          null,            null,            7,               null,               2,                0,                    0                }, 
    { "int2",           5,          19,             null,           null,             null,                     1,          0,                2,            0,                    10,                 0,                   "int2",            0,               0,               5,               null,               2,                0,                    0                }, 
    { "int2",           -6,         5,              null,           null,             null,                     1,          0,                2,            0,                    10,                 0,                   "int2",            0,               0,               5,               null,               2,                0,                    0                }, 
    { "timestamp",      11,         26,             "'",            "'",              null,                     1,          0,                2,            null,                 0,                  null,                "timestamp",       0,               38,              9,               3,                  null,             0,                    0                }, 
    { "date",           91,         10,             "'",            "'",              null,                     1,          0,                2,            null,                 0,                  null,                "date",            null,            null,            9,               1,                  null,             0,                    0                }, 
    { "timestamp",      93,         26,             "'",            "'",              null,                     1,          0,                2,            null,                 0,                  null,                "timestamp",       0,               38,              9,               3,                  null,             0,                    0                }, 
    { "bytea",          -3,         255,            "'",            "'",              null,                     1,          0,                2,            null,                 0,                  null,                "bytea",           null,            null,            -3,              null,               null,             0,                    0                }, 
    { "varchar",        12,         65535,          "'",            "'",              "max. length",            1,          0,                2,            null,                 0,                  null,                "varchar",         null,            null,           -9,               null,               null,             0,                    0                }, 
    { "char",           -8,         65535,          "'",            "'",              "max. length",            1,          1,                3,            null,                 0,                  null,                "char",            null,            null,           -8,               null,               null,             0,                    0                }, 
    { "text",           -10,        65535,          "'",            "'",              "max. length",            1,          1,                3,            null,                 0,                  null,                "text",            null,            null,           -10,              null,               null,             0,                    0                }, 
    { "varchar",        -9,         65535,          "'",            "'",              "max. length",            1,          1,                3,            null,                 0,                  null,                "varchar",         null,            null,           -9,               null,               null,             0,                    0                },

    // Added:
    { "bpchar",         -8,         65535,           "'",            "'",              "max. length",            1,          1,                3,            null,                 0,                  null,                "bpchar",          null,            null,            -9,               null,               null,            0,                    0                } }
                        
    // Removed since there is no such data type in Redshift and producing literals for it results in errors:
    //{ "time",           10,         8,              "'",            "'",              null,                     1,          0,                2,            null,                 0,                  null,                "time",            0,               0,               9,               2,                  null,             0,                    0                }, 
    //{ "time",           92,         8,              "'",            "'",              null,                     1,          0,                2,            null,                 0,                  null,                "time",            0,               0,               9,               2,                  null,             0,                    0                }, 
);

[DataSource.Kind="RedshiftODBC", Publish="RedshiftODBC.UI"]
shared RedshiftODBC.Database = (server as text, database as text, optional options as record) as table =>
    let
        ConnectionString = GetAddress(server) & [
            Driver = "Amazon Redshift (x64)",
            UseUnicode = "yes",
            Database = database
        ],
        Credential = Extension.CurrentCredential(),
        encryptionEnabled = Credential[EncryptConnection]? = true,
        Options = [
            CredentialConnectionString = [
                SSLMode = if encryptionEnabled then "verify-full" else "prefer",
                UID = Credential[Username],
                PWD = Credential[Password],
                BoolsAsChar = 0,
                MaxVarchar = 65535
            ],
            ClientConnectionPooling = true,
            UseEmbeddedDriver = true,
            OnError = OnError
        ],
        OdbcDataSource = Odbc.DataSource(ConnectionString, Options & [
            HierarchicalNavigation = true,
            SqlCapabilities = [
                SupportsTop = true,
                Sql92Conformance = 8 /* SQL_SC_SQL92_FULL */,
                GroupByCapabilities = 4 /* SQL_GB_NO_RELATION */,
                FractionalSecondsScale = 3,

                // Driver doesn't support parameters
                SupportsNumericLiterals = true,
                SupportsStringLiterals = true,
                StringLiteralEscapeCharacters = { "\" },
                SupportsOdbcDateLiterals = true,
                SupportsOdbcTimeLiterals = true,
                SupportsOdbcTimestampLiterals = true
            ],
            SQLGetFunctions = [
                // Driver doesn't support parameters
                SQL_API_SQLBINDPARAMETER = false
            ],

            HideNativeQuery = true,

            // Needed to work around driver behavior where
            // CONVERT is not translated properly by the driver in JOIN ON clauses.
            ImplicitTypeConversions = ImplicitTypeConversions,
            SQLGetTypeInfo = SQLGetTypeInfo,

            AstVisitor = [
                // Decimal literals are always of type "numeric" which prevents high range numbers to be used.
                // We therefore wrap float4 and float8 with CAST for that purpose.
                Constant =
                    let
                        Cast = (value, typeName) => [
                            Text = Text.Format("CAST(#{0} as #{1})", { value, typeName })
                        ],
                        Visitor = [
                            float8 = each Cast(_, "float8"),
                            float4 = each Cast(_, "float4")
                        ]
                    in
                        (typeInfo, ast) => Record.FieldOrDefault(Visitor, typeInfo[TYPE_NAME], each null)(ast[Value])
            ]
        ]),

        Database = OdbcDataSource{[Name = database]}[Data],
        RemovedSystemTable = Table.SelectRows(Database, each [Name] <> "pg_internal"), // non-queryable system table
        FixNestedNavigationTables =
            let
                ColumnType = Type.TableColumn(Value.Type(RemovedSystemTable), "Data"),
                AddColumn = Table.AddColumn(RemovedSystemTable, "Data2", each FixNavigationTable([Data]), ColumnType),
                RemovePreviousColumn = Table.RemoveColumns(AddColumn, { "Data" }),
                RenameColumn = Table.RenameColumns(RemovePreviousColumn, { { "Data2", "Data" } })
            in
                RenameColumn,
        Fix = FixNavigationTable(FixNestedNavigationTables),
                    
        OnError = (errorRecord as record) =>
            if Text.Contains(errorRecord[Message], "password authentication failed") then
                error Extension.CredentialError(Credential.AccessDenied, errorRecord[Message])
            else if encryptionEnabled and Text.Contains(errorRecord[Message], "root.crt"" does not exist") then
                error Extension.CredentialError(Credential.EncryptionNotSupported, errorRecord[Message])
            else 
                error errorRecord,

        GetAddress = (server as text) as record =>
            let
                Address = Uri.Parts("http://" & server),
                Port = if Address[Port] = 80 and not Text.EndsWith(server, ":80") then [] else [Port = Address[Port]],
                Server = [Server = Address[Host]],
                ConnectionString = Server & Port,
                Result =
                    if Address[Host] = ""
                        or Address[Scheme] <> "http"
                        or Address[Path] <> "/"
                        or Address[Query] <> []
                        or Address[Fragment] <> ""
                        or Address[UserName] <> ""
                        or Address[Password] <> ""
                        or Text.StartsWith(server, "http:/", Comparer.OrdinalIgnoreCase) then
                        error "Invalid server name"
                    else
                        ConnectionString
            in
                Result,

        // Fixes navigation table return from ODBC hierarchical navigation
        // to not have Kind as part of the key and to remove columns that return
        // not value for this ODBC driver such as "Description".
        FixNavigationTable = (table) =>
            let
                SelectColumns = Table.SelectColumns(table, { "Name", "Data", "Kind" }),
                OriginalType = Value.Type(SelectColumns),
                Type = type table [
                    Name = Type.TableColumn(OriginalType, "Name"),
                    Data = Type.TableColumn(OriginalType, "Data"),
                    Kind = Type.TableColumn(OriginalType, "Kind")
                ],
                AddKey = Type.AddTableKey(Type, { "Name" }, true),
                AddMetadata = AddKey meta [
                    NavigationTable.NameColumn = "Name",
                    NavigationTable.DataColumn = "Data",
                    NavigationTable.KindColumn = "Kind",
                    Preview.DelayColumn = "Data"
                ],
                ReplaceType = Value.ReplaceType(SelectColumns, AddMetadata)
            in
                ReplaceType
    in
        Fix;


RedshiftODBC = [
    Authentication = [
        UsernamePassword = [
        ]
    ],
    SupportsEncryption = true
];

RedshiftODBC.UI = [
    ButtonText = { "RedshiftODBC Sample", "RedshiftODBC Sample" },
    Category = "Database",
    SupportsDirectQuery = true
];