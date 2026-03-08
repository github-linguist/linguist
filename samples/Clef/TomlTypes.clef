/// TOML value types for representing parsed TOML documents.
/// Fully compliant with TOML 1.0.0 specification.
/// Designed to be BCL-minimal, to support Clef dimensional types.
namespace Fidelity.Data.TOML

/// Represents a TOML date-time value with optional timezone offset.
[<Struct>]
type TomlDateTime =
    { Year: int
      Month: int
      Day: int
      Hour: int
      Minute: int
      Second: int
      Nanosecond: int
      /// Offset from UTC in minutes. None for local date-times.
      OffsetMinutes: int voption }

/// Represents a TOML local date (no time component).
[<Struct>]
type TomlLocalDate =
    { Year: int
      Month: int
      Day: int }

/// Represents a TOML local time (no date component).
[<Struct>]
type TomlLocalTime =
    { Hour: int
      Minute: int
      Second: int
      Nanosecond: int }

/// Represents any TOML value type.
[<RequireQualifiedAccess>]
type TomlValue =
    /// A string value (basic or literal, single or multiline).
    | String of string
    /// A 64-bit signed integer.
    | Integer of int64
    /// A 64-bit floating point number.
    | Float of float
    /// A boolean value.
    | Boolean of bool
    /// An offset date-time (with timezone).
    | OffsetDateTime of TomlDateTime
    /// A local date-time (no timezone).
    | LocalDateTime of TomlDateTime
    /// A local date (no time component).
    | LocalDate of TomlLocalDate
    /// A local time (no date component).
    | LocalTime of TomlLocalTime
    /// An array of values.
    | Array of TomlValue list
    /// An inline table.
    | InlineTable of TomlTable
    /// A regular table (from [table] header).
    | Table of TomlTable

/// Represents a TOML table (key-value pairs) using Map.
and TomlTable = Map<string, TomlValue>

/// Represents a complete TOML document.
type TomlDocument = TomlTable

/// Module for working with TOML tables.
module TomlTable =
    /// Creates an empty TOML table.
    let empty: TomlTable = Map.empty

    /// Adds a key-value pair to a table.
    let add (key: string) (value: TomlValue) (table: TomlTable): TomlTable =
        Map.add key value table

    /// Tries to get a value by key.
    let tryFind (key: string) (table: TomlTable): TomlValue option =
        Map.tryFind key table

    /// Gets all keys in the table.
    let keys (table: TomlTable): string list =
        table |> Map.toList |> List.map fst

    /// Checks if a key exists.
    let containsKey (key: string) (table: TomlTable): bool =
        Map.containsKey key table

/// Module for working with TOML documents.
module TomlDocument =
    /// Creates an empty TOML document.
    let empty: TomlDocument = TomlTable.empty

    /// Navigates to a nested value using a dotted key path.
    /// For example, "package.name" navigates to table["package"]["name"].
    let tryGetPath (path: string) (doc: TomlDocument): TomlValue option =
        let keys = path.Split('.') |> Array.toList
        let rec navigate (keys: string list) (current: TomlValue): TomlValue option =
            match keys with
            | [] -> Some current
            | key :: rest ->
                match current with
                | TomlValue.Table t
                | TomlValue.InlineTable t ->
                    match TomlTable.tryFind key t with
                    | Some v -> navigate rest v
                    | None -> None
                | _ -> None

        match keys with
        | [] -> None
        | firstKey :: rest ->
            match TomlTable.tryFind firstKey doc with
            | Some v ->
                if List.isEmpty rest then Some v
                else navigate rest v
            | None -> None

    /// Gets a string value by dotted key path.
    let tryGetString (path: string) (doc: TomlDocument): string option =
        match tryGetPath path doc with
        | Some (TomlValue.String s) -> Some s
        | _ -> None

    /// Gets an integer value by dotted key path.
    let tryGetInt (path: string) (doc: TomlDocument): int64 option =
        match tryGetPath path doc with
        | Some (TomlValue.Integer i) -> Some i
        | _ -> None

    /// Gets a float value by dotted key path.
    let tryGetFloat (path: string) (doc: TomlDocument): float option =
        match tryGetPath path doc with
        | Some (TomlValue.Float f) -> Some f
        | _ -> None

    /// Gets a boolean value by dotted key path.
    let tryGetBool (path: string) (doc: TomlDocument): bool option =
        match tryGetPath path doc with
        | Some (TomlValue.Boolean b) -> Some b
        | _ -> None

    /// Gets a string array by dotted key path.
    let tryGetStringArray (path: string) (doc: TomlDocument): string list option =
        match tryGetPath path doc with
        | Some (TomlValue.Array arr) ->
            let strings =
                arr
                |> List.choose (function TomlValue.String s -> Some s | _ -> None)
            if List.length strings = List.length arr then Some strings
            else None
        | _ -> None

    /// Gets a table by dotted key path.
    let tryGetTable (path: string) (doc: TomlDocument): TomlTable option =
        match tryGetPath path doc with
        | Some (TomlValue.Table t) -> Some t
        | Some (TomlValue.InlineTable t) -> Some t
        | _ -> None

    /// Gets an inline table value by key path.
    let tryGetInlineTable (path: string) (doc: TomlDocument): (string * TomlValue) list option =
        match tryGetPath path doc with
        | Some (TomlValue.InlineTable t) ->
            t |> Map.toList |> Some
        | _ -> None
