# From https://github.com/agu-z/roc-pg/blob/main/src/Cmd.roc
# License is UPL
interface Cmd exposes
    [
        Cmd,
        Params,
        Limit,
        Kind,
        fromSql,
        prepared,
        params,
        withLimit,
        decode,
        withDecode,
        map,
        bind,
        Binding,
        encodeBindings,
    ] imports [
        Protocol.Frontend.{ FormatCode },
        Protocol.Backend.{ RowField },
        Pg.Result.{ CmdResult },
    ]

Cmd a err := Params { decode : CmdResult -> Result a err } []

Limit : [None, Limit I32]

Params p k : {
    kind : Kind k,
    bindings : List Binding,
    limit : Limit,
}p

Kind k : [
    SqlCmd Str,
    PreparedCmd
        {
            name : Str,
            fields : List RowField,
        },
]k

fromSql : Str -> Cmd CmdResult []
fromSql = \sql ->
    new (SqlCmd sql)

prepared : { name : Str, fields : List RowField } -> Cmd CmdResult []
prepared = \prep ->
    new (PreparedCmd prep)

new : Kind [] -> Cmd CmdResult []
new = \kind ->
    @Cmd {
        kind,
        limit: None,
        bindings: [],
        decode: Ok,
    }

params : Cmd a err -> Params {} []
params = \@Cmd { kind, bindings, limit } ->
    { kind, bindings, limit }

withLimit : Cmd a err, I32 -> Cmd a err
withLimit = \@Cmd cmd, limit ->
    @Cmd { cmd & limit: Limit limit }

decode : CmdResult, Cmd a err -> Result a err
decode = \r, @Cmd cmd ->
    cmd.decode r

withDecode : Cmd * *, (CmdResult -> Result a err) -> Cmd a err
withDecode = \@Cmd cmd, fn ->
    @Cmd {
        kind: cmd.kind,
        limit: cmd.limit,
        bindings: cmd.bindings,
        decode: fn,
    }

map : Cmd a err, (a -> b) -> Cmd b err
map = \@Cmd cmd, fn ->
    @Cmd {
        kind: cmd.kind,
        limit: cmd.limit,
        bindings: cmd.bindings,
        decode: \r -> cmd.decode r |> Result.map fn,
    }

bind : Cmd a err, List Binding -> Cmd a err
bind = \@Cmd cmd, bindings ->
    @Cmd { cmd & bindings }

Binding : [
    Null,
    Text Str,
    Binary (List U8),
]

encodeBindings : List Binding
    -> {
        formatCodes : List FormatCode,
        paramValues : List [Null, Value (List U8)],
    }
encodeBindings = \bindings ->
    count = List.len bindings

    empty = {
        formatCodes: List.withCapacity count,
        paramValues: List.withCapacity count,
    }

    List.walk bindings empty \state, binding ->
        { format, value } = encodeSingle binding

        {
            formatCodes: state.formatCodes |> List.append format,
            paramValues: state.paramValues |> List.append value,
        }

encodeSingle = \binding ->
    when binding is
        Null ->
            {
                value: Null,
                format: Binary,
            }

        Binary value ->
            {
                value: Value value,
                format: Binary,
            }

        Text value ->
            {
                value: Value (Str.toUtf8 value),
                format: Text,
            }
