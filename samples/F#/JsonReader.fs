namespace Nessos.FsPickler.Json

    open System
    open System.Collections.Generic
    open System.Globalization
    open System.IO
    open System.Numerics
    open System.Text

    open Newtonsoft.Json

    open Nessos.FsPickler

    /// <summary>
    ///     Json format deserializer
    /// </summary>
    type internal JsonPickleReader (jsonReader : JsonReader, omitHeader, isTopLevelSequence, leaveOpen) =

        do
            jsonReader.CloseInput <- not leaveOpen
            jsonReader.SupportMultipleContent <- isTopLevelSequence

        let isBsonReader = match jsonReader with :? Bson.BsonReader -> true | _ -> false

        let mutable depth = 0
        let arrayStack = new Stack<int> ()
        do arrayStack.Push Int32.MinValue

        // do not write tag if omitting header or array element
        let omitTag () = (omitHeader && depth = 0) || arrayStack.Peek() = depth - 1

        interface IPickleFormatReader with
            
            member __.BeginReadRoot (tag : string) =
                do jsonReader.Read() |> ignore
                    
                if omitHeader then () else

                if jsonReader.TokenType <> JsonToken.StartObject then raise <| new FormatException("invalid json root object.")
                else
                    do jsonReader.MoveNext()
                    let version = jsonReader.ReadPrimitiveAs<string> false "FsPickler"
                    if version <> jsonFormatVersion then
                        let v = Version(version)
                        raise <| new FormatException(sprintf "Invalid FsPickler format version %O." version)

                    let sTag = jsonReader.ReadPrimitiveAs<string> false "type"
                    if tag <> sTag then
                        raise <| new InvalidPickleTypeException(tag, sTag)

            member __.EndReadRoot () = 
                if not omitHeader then jsonReader.Read() |> ignore

            member __.BeginReadObject (tag : string) =
                
                if not <| omitTag () then
                    jsonReader.ReadProperty tag
                    jsonReader.MoveNext ()

                if isTopLevelSequence && depth = 0 then
                    arrayStack.Push depth
                    depth <- depth + 1
                    ObjectFlags.IsSequenceHeader

                else
                    match jsonReader.TokenType with
                    | JsonToken.Null -> ObjectFlags.IsNull
                    | JsonToken.StartArray ->
                        jsonReader.MoveNext()
                        arrayStack.Push depth
                        depth <- depth + 1
                        ObjectFlags.IsSequenceHeader

                    | JsonToken.StartObject ->
                        do jsonReader.MoveNext()
                        depth <- depth + 1

                        if jsonReader.ValueAs<string> () = "_flags" then
                            jsonReader.MoveNext()
                            let csvFlags = jsonReader.ValueAs<string>()
                            jsonReader.MoveNext()
                            parseFlagCsv csvFlags
                        else
                            ObjectFlags.None

                    | token -> raise <| new FormatException(sprintf "expected start of Json object but was '%O'." token)


            member __.EndReadObject () =
                if isTopLevelSequence && depth = 1 then
                    arrayStack.Pop () |> ignore
                    depth <- depth - 1
                    jsonReader.Read() |> ignore
                else
                    match jsonReader.TokenType with
                    | JsonToken.Null -> ()
                    | JsonToken.EndObject -> depth <- depth - 1
                    | JsonToken.EndArray ->
                        arrayStack.Pop() |> ignore
                        depth <- depth - 1

                    | token -> raise <| new FormatException(sprintf "expected end of Json object but was '%O'." token)

                    if omitHeader && depth = 0 then ()
                    else jsonReader.Read() |> ignore

            member __.SerializeUnionCaseNames = true

            member __.PreferLengthPrefixInSequences = false
            member __.ReadNextSequenceElement () = 
                if isTopLevelSequence && depth = 1 then
                    jsonReader.TokenType <> JsonToken.None
                else
                    jsonReader.TokenType <> JsonToken.EndArray

            member __.ReadCachedObjectId () = jsonReader.ReadPrimitiveAs<int64> false "id"

            member __.ReadBoolean tag = jsonReader.ReadPrimitiveAs<bool> (omitTag ()) tag
            member __.ReadByte tag = jsonReader.ReadPrimitiveAs<int64> (omitTag ()) tag |> byte
            member __.ReadSByte tag = jsonReader.ReadPrimitiveAs<int64> (omitTag ()) tag |> sbyte

            member __.ReadInt16 tag = jsonReader.ReadPrimitiveAs<int64> (omitTag ()) tag |> int16
            member __.ReadInt32 tag = jsonReader.ReadPrimitiveAs<int64> (omitTag ()) tag |> int
            member __.ReadInt64 tag = jsonReader.ReadPrimitiveAs<int64> (omitTag ()) tag

            member __.ReadUInt16 tag = jsonReader.ReadPrimitiveAs<int64> (omitTag ()) tag |> uint16
            member __.ReadUInt32 tag = jsonReader.ReadPrimitiveAs<int64> (omitTag ()) tag |> uint32
            member __.ReadUInt64 tag = jsonReader.ReadPrimitiveAs<int64> (omitTag ()) tag |> uint64

            member __.ReadSingle tag =
                if not <| omitTag () then
                    jsonReader.ReadProperty tag
                    jsonReader.MoveNext()

                let value =
                    match jsonReader.TokenType with
                    | JsonToken.Float -> jsonReader.ValueAs<double> () |> single
                    | JsonToken.String -> Single.Parse(jsonReader.ValueAs<string>(), CultureInfo.InvariantCulture)
                    | _ -> raise <| new FormatException("not a float.")

                jsonReader.Read() |> ignore
                value
                
            member __.ReadDouble tag =
                if not <| omitTag () then
                    jsonReader.ReadProperty tag
                    jsonReader.MoveNext()

                let value =
                    match jsonReader.TokenType with
                    | JsonToken.Float -> jsonReader.ValueAs<double> ()
                    | JsonToken.String -> Double.Parse(jsonReader.ValueAs<string>(), CultureInfo.InvariantCulture)
                    | _ -> raise <| new FormatException("not a float.")

                jsonReader.Read() |> ignore
                value

            member __.ReadChar tag = let value = jsonReader.ReadPrimitiveAs<string> (omitTag ()) tag in value.[0]
            member __.ReadString tag = jsonReader.ReadPrimitiveAs<string> (omitTag ()) tag
            member __.ReadBigInteger tag = jsonReader.ReadPrimitiveAs<string> (omitTag ()) tag |> BigInteger.Parse

            member __.ReadGuid tag = 
                if isBsonReader then 
                    jsonReader.ReadPrimitiveAs<Guid> (omitTag ()) tag
                else
                    jsonReader.ReadPrimitiveAs<string> (omitTag ()) tag |> Guid.Parse

            member __.ReadTimeSpan tag = jsonReader.ReadPrimitiveAs<string> (omitTag ()) tag |> TimeSpan.Parse
            member __.ReadDecimal tag = jsonReader.ReadPrimitiveAs<string> (omitTag ()) tag |> decimal

            // BSON spec mandates the use of Unix time; 
            // this has millisecond precision which results in loss of accuracy w.r.t. ticks
            // since the goal of FsPickler is to offer faithful representations of .NET objects
            // we choose to override the spec and serialize ticks outright.
            // see also https://json.codeplex.com/discussions/212067 
            member __.ReadDate tag = 
                if isBsonReader then
                    let ticks = jsonReader.ReadPrimitiveAs<int64> (omitTag ()) tag
                    DateTime(ticks)
                else
                    jsonReader.ReadPrimitiveAs<DateTime> (omitTag ()) tag

            member __.ReadBytes tag =
                if not <| omitTag () then
                    jsonReader.ReadProperty tag
                    jsonReader.Read() |> ignore

                let bytes =
                    if jsonReader.TokenType = JsonToken.Null then null
                    elif isBsonReader then jsonReader.ValueAs<byte []> ()
                    else
                        let base64 = jsonReader.ValueAs<string> ()
                        Convert.FromBase64String base64

                jsonReader.Read() |> ignore

                bytes

            member __.IsPrimitiveArraySerializationSupported = false
            member __.ReadPrimitiveArray _ _ = raise <| new NotImplementedException()

            member __.Dispose () = (jsonReader :> IDisposable).Dispose()