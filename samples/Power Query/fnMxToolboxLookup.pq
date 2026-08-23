/*
==============================================================================
 Function Name : fnMxToolboxLookup
 SOAR Stage    : SOAR 1 – API Lookup (Inner)
 Returns       : table (canonical schema; 1 row)
 Purpose       : Call MXToolbox API and return a single-row table normalized
                 to the canonical schema contract WITHOUT type/value leakage.

 Dependencies  :
   - pMxApiKey
   - q_MxSchema_Contract

 Guardrails :
   - MUST return a TABLE value (never a naked type)
   - MUST NOT forward-reference values
   - MUST enforce schema using a LIST of {ColumnName, Type} pairs
==============================================================================
*/
(domain as text, command as text) =>
let
    BaseUrl = "https://api.mxtoolbox.com/api/v1/lookup/" & command,
    Url = BaseUrl & "?" & Uri.BuildQueryString([ argument = domain ]),
    Headers = [
        Accept = "application/json",
        Authorization = pMxApiKey
    ],

    Response =
        Web.Contents(
            Url,
            [ Headers = Headers ]
        ),

    Json = Json.Document(Response),

    UIDVal = try Json[UID] otherwise null,
    CommandArgumentVal = try Json[CommandArgument] otherwise domain,
    CommandValue = try Json[Command] otherwise command,
    MxRepValue = try Json[MxRep] otherwise null,
    DnsProviderVal = try Json[DnsServiceProvider] otherwise null,
    TimeRecordedRaw = try Json[TimeRecorded] otherwise null,
    ReportingNSVal = try Json[ReportingNameServer] otherwise null,
    TimeToCompleteVal = try Json[TimeToComplete] otherwise null,
    IsEndpointVal = try Json[IsEndpoint] otherwise null,
    HasSubscriptionsVal = try Json[HasSubscriptions] otherwise null,

    TimeRecordedVal =
        try DateTimeZone.From(TimeRecordedRaw)
        otherwise try DateTime.From(TimeRecordedRaw)
        otherwise null,

    FailedRaw = try Json[Failed] otherwise null,
    WarningsRaw = try Json[Warnings] otherwise null,
    PassedRaw = try Json[Passed] otherwise null,
    TimeoutsRaw = try Json[Timeouts] otherwise null,

    FailedList = if FailedRaw is list then FailedRaw else {},
    WarningsList = if WarningsRaw is list then WarningsRaw else {},
    PassedList = if PassedRaw is list then PassedRaw else {},
    TimeoutsList = if TimeoutsRaw is list then TimeoutsRaw else {},

    FailedCount = List.Count(FailedList),
    WarningsCount = List.Count(WarningsList),
    PassedCount = List.Count(PassedList),
    TimeoutsCount = List.Count(TimeoutsList),

    HasDmarcRecord =
        if Text.Lower(command) = "dmarc" then FailedCount = 0 else null,

    OutputRecord = [
        UID = UIDVal,
        Domain = CommandArgumentVal,
        Command = CommandValue,
        CommandArgument = CommandArgumentVal,
        MxRep = MxRepValue,
        DnsProvider = DnsProviderVal,
        ReportingNameServer = ReportingNSVal,
        TimeRecorded = TimeRecordedVal,
        TimeToComplete = TimeToCompleteVal,
        IsEndpoint = IsEndpointVal,
        HasSubscriptions = HasSubscriptionsVal,
        FailedCount = FailedCount,
        WarningsCount = WarningsCount,
        PassedCount = PassedCount,
        TimeoutsCount = TimeoutsCount,
        FailedList = FailedList,
        WarningsList = WarningsList,
        PassedList = PassedList,
        TimeoutsList = TimeoutsList,
        HasDmarcRecord = HasDmarcRecord
    ],

    OutputTable =
        #table(
            Record.FieldNames(OutputRecord),
            { Record.FieldValues(OutputRecord) }
        ),

    Renamed =
        Table.RenameColumns(
            OutputTable,
            {{"Command", "MxCommand"}},
            MissingField.Ignore
        ),

    /* Build a valid TransformColumnTypes mapping from q_MxSchema_Contract */
    SchemaFieldRecord =
        Type.RecordFields(
            Type.TableRow(
                Value.Type(q_MxSchema_Contract)
            )
        ),

    FullTypeMap =
        List.Transform(
            Record.FieldNames(SchemaFieldRecord),
            (colName) => { colName, Record.Field(SchemaFieldRecord, colName)[Type] }
        ),

    /* Only apply types for columns that actually exist in this table */
    PresentCols = Table.ColumnNames(Renamed),
    TypeMapFiltered =
        List.Select(
            FullTypeMap,
            (pair) => List.Contains(PresentCols, pair{0})
        ),

    Normalized =
        Table.TransformColumnTypes(
            Renamed,
            TypeMapFiltered
        )
in
    Normalized
