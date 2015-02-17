namespace Nessos.FsPickler.Tests

    open System
    open System.Collections.Generic

    open PerfUtil

    open Nessos.FsPickler
    open Nessos.FsPickler.Tests.Serializer
    open Nessos.FsPickler.Tests.TestTypes

    module PerformanceTests =

        type Marker = class end

        let guid = Guid.NewGuid()

        [<PerfTest(1000)>]
        let ``Value: Guid`` s = roundtrip guid s

        let date = DateTime.Now

        [<PerfTest(1000)>]
        let ``Value: DateTime`` s = roundtrip date s

        [<PerfTest(10000)>]
        let ``Value: String`` s = roundtrip stringValue s


        let boxed = box ([| 1 .. 1000 |], "lorem ipsum")

        [<PerfTest(1000)>]
        let ``Boxed Object`` s = roundtrip boxed s

        let fsClass = new Class(42, stringValue)

        [<PerfTest(10000)>]
        let ``Class: Simple F# Class`` s = roundtrip fsClass s

        let serializableClass = new SerializableClass<_>(42, stringValue, [|1..1000|])

        [<PerfTest(10000)>]
        let ``Class: ISerializable`` s = roundtrip serializableClass s

        let boxedClass = box(Some 42)

        [<PerfTest(10000)>]
        let ``Subtype Resolution`` s = roundtrip boxedClass s

        let floatArray = Array.init 100000 (fun i -> float i)

        [<PerfTest(10)>]
        let ``Array: Float`` s = roundtrip floatArray s

        let intArray = Array.init 100000 id

        [<PerfTest(10)>]
        let ``Array: Int`` s = roundtrip intArray s

        let stringArray = Array.init 10000 (fun i -> stringValue + string i)
        
        [<PerfTest(100)>]
        let ``Array: String`` s = roundtrip stringArray s

        let kvarr = [|1..10000|] |> Array.map (fun i -> i, string i)

        [<PerfTest(100)>]
        let ``Array: Key-Value Pairs`` s = roundtrip kvarr s

        let duArray = [| for i in 1 .. 10000 -> (Something ("asdasdasdas", i)) |]

        [<PerfTest(100)>]
        let ``Array: Discriminated Unions`` s = roundtrip duArray s

        let objArray = 
            [| 
                box 2; box 3; box "hello" ; box <| Some 3; box(2,3) ; 
                box <| new Class(2, stringValue) ; box <| new SerializableClass<int option>(2, stringValue, Some 12); 
                box stringValue 
            |]

        [<PerfTest(1000)>]
        let ``Array: Objects`` s = roundtrip objArray s


        let array3D = Array3D.init 100 100 100 (fun i j k -> float (i * j + k))

        [<PerfTest(10)>]
        let ``Array: Rank-3 Float`` s = roundtrip array3D s

        let bclDict = dict [ for i in 1 .. 1000 -> (string i, i)]

        [<PerfTest(100)>]
        let ``.NET Dictionary`` s = roundtrip bclDict s

        let bclStack = new Stack<string>([for i in 1 .. 1000 -> string i])

        [<PerfTest(100)>]
        let ``.NET Stack`` s = roundtrip bclStack s

        let bclList = new List<string * int>([for i in 1 .. 1000 -> string i, i])

        [<PerfTest(100)>]
        let ``.NET List`` s = roundtrip bclList s

        let bclSet = new SortedSet<_>([for i in 1 .. 1000 -> string i])

        [<PerfTest(100)>]
        let ``.NET Set`` s = roundtrip bclSet s

        let smallTuple = (1, DateTime.Now,"hello")

        [<PerfTest(10000)>]
        let ``FSharp: Tuple Small`` s = roundtrip smallTuple s

        let largeTuple = (stringValue, 1, 2, 3, true, "", Some(3.14, [2]), 3, 2, 1, stringValue)

        [<PerfTest(10000)>]
        let ``FSharp: Tuple Large`` s =
            roundtrip largeTuple s

        let intList = [1..1000]

        [<PerfTest(1000)>]
        let ``FSharp: List Int`` s = roundtrip intList s

        let stringList = [ for i in 1 .. 1000 -> stringValue + string i ]

        [<PerfTest(1000)>]
        let ``FSharp: List String`` s = roundtrip stringList s

        let pairList = [ for i in 1 .. 1000 -> (string i, i) ]

        [<PerfTest(1000)>]
        let ``FSharp: List Key-Value`` s = roundtrip pairList s

        let nestedLst = let n = [1..1000] in [for _ in 1 .. 100 -> n]

        [<PerfTest(1000)>]
        let ``FSharp: List Nested`` s = roundtrip nestedLst s

        let union = SomethingElse(stringValue, 42, box (Some 42))

        [<PerfTest(10000)>]
        let ``FSharp: Union`` s = roundtrip union s

        let record = { Int = 42 ; String = stringValue ; Tuple = (13, "") }

        [<PerfTest(10000)>]
        let ``FSharp: Record`` s = roundtrip record s

        let peano = int2Peano 100

        [<PerfTest(100)>]
        let ``FSharp: Peano Rectype`` s = roundtrip peano s

        let closure = (@) [ Some([1..100], Set.ofList [1..100]) ]

        [<PerfTest(1000)>]
        let ``FSharp: Curried Function`` s = roundtrip closure s

        let binTree = mkTree 10

        [<PerfTest(100)>]
        let ``FSharp: Binary Tree`` s = roundtrip binTree s

        let intSet = [1..1000] |> List.map string |> set

        [<PerfTest(1000)>]
        let ``FSharp: Set`` s = roundtrip intSet s

        let fsMap = [1..1000] |> Seq.map (fun i -> (string i,i)) |> Map.ofSeq

        [<PerfTest(1000)>]
        let ``FSharp: Map`` s = roundtrip fsMap s

        let testType = typeof<int * string option * Map<int * string [], string ref option>>

        [<PerfTest(1000)>]
        let ``Reflection: Type`` s = roundtrip testType s

        let quotationSmall = <@ fun x -> pown 2 x @>

        let quotationLarge =
            <@
                async {
                    let rec fibAsync n =
                        async {
                            match n with
                            | _ when n < 0 -> return invalidArg "negative" "n"
                            | _ when n < 2 -> return n
                            | n ->
                                let! fn = fibAsync (n-1)
                                let! fnn = fibAsync (n-2)
                                return fn + fnn
                        }

                    let! values = [1..100] |> Seq.map fibAsync |> Async.Parallel
                    return Seq.sum values
                }
            @>

        [<PerfTest(10000)>]
        let ``FSharp: Quotation Small`` s = roundtrip quotationSmall s

        [<PerfTest(1000)>]
        let ``FSharp: Quotation Large`` s = roundtrip quotationLarge s