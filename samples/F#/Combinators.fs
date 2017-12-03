namespace Nessos.FsPickler.Combinators

    open Nessos.FsPickler
    open Nessos.FsPickler.Json

    /// Json pickling methods
    [<RequireQualifiedAccess>]
    module Json =

        let private jsonSerializer = lazy(FsPickler.CreateJson(omitHeader = true))

        /// <summary>
        ///     Pickles a value to Json.
        /// </summary>
        /// <param name="pickler">utilized pickler.</param>
        /// <param name="value">input value.</param>
        let pickle (pickler : Pickler<'T>) (value : 'T) : string =
            jsonSerializer.Value.PickleToString (pickler, value)

        /// <summary>
        ///     Unpickles a value from Json.
        /// </summary>
        /// <param name="pickler">utilized pickler.</param>
        /// <param name="pickle">input pickle.</param>
        let unpickle (pickler : Pickler<'T>) (pickle : string) : 'T =
            jsonSerializer.Value.UnPickleOfString (pickler, pickle)


    /// Bson pickling methods
    [<RequireQualifiedAccess>]
    module Bson =

        let private bsonPickler = lazy(FsPickler.CreateBson())

        /// <summary>
        ///     Pickles a value to Bson.
        /// </summary>
        /// <param name="pickler">utilized pickler.</param>
        /// <param name="value">input value.</param>
        let pickle (pickler : Pickler<'T>) (value : 'T) : byte [] =
            bsonPickler.Value.Pickle (pickler, value)

        /// <summary>
        ///     Unpickles a value from bson.
        /// </summary>
        /// <param name="pickler">utilized pickler.</param>
        /// <param name="pickle">input pickle.</param>
        let unpickle (pickler : Pickler<'T>) (pickle : byte []) : 'T =
            bsonPickler.Value.UnPickle (pickler, pickle)