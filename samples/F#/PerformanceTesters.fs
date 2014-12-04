namespace Nessos.FsPickler.Tests

    open PerfUtil
    open PerfUtil.NUnit

    open NUnit.Framework

    open Nessos.FsPickler
    open Nessos.FsPickler.Json

    [<AbstractClass>]
    type PerfTester () =
        inherit NUnitPerf<Serializer> ()

        let tests = PerfTest.OfModuleMarker<PerformanceTests.Marker> ()

        override __.PerfTests = tests


    type ``Serializer Comparison`` () =
        inherit PerfTester()

        let fsp = FsPickler.initBinary()
        let bfs = new BinaryFormatterSerializer() :> Serializer
        let ndc = new NetDataContractSerializer() :> Serializer
        let jdn = new JsonDotNetSerializer() :> Serializer
        let bdn = new JsonDotNetBsonSerializer () :> Serializer
        let pbn = new ProtoBufSerializer() :> Serializer
        let ssj = new ServiceStackJsonSerializer() :> Serializer
        let sst = new ServiceStackTypeSerializer() :> Serializer

        let comparer = new WeightedComparer(spaceFactor = 0.2, leastAcceptableImprovementFactor = 1.)
        let tester = new ImplementationComparer<_>(fsp, [bfs;ndc;jdn;bdn;pbn;ssj;sst], throwOnError = true, warmup = true, comparer = comparer)

        override __.PerfTester = tester :> _
        

    type ``FsPickler Formats Comparison`` () =
        inherit PerfTester ()

        let binary = FsPickler.initBinary()
        let json = FsPickler.initJson()
        let bson = FsPickler.initBson()
        let xml = FsPickler.initXml()

        let tester = new ImplementationComparer<_>(binary, [json ; bson; xml], warmup = true, throwOnError = false)

        override __.PerfTester = tester :> _


    type ``Past FsPickler Versions Comparison`` () =
        inherit PerfTester ()

        let persistResults = true
        let persistenceFile = "fspPerf.xml"

        let fsp = FsPickler.initBinary()
        let version = typeof<FsPickler>.Assembly.GetName().Version
        let comparer = new WeightedComparer(spaceFactor = 0.2, leastAcceptableImprovementFactor = 0.8)
        let tester = 
            new PastImplementationComparer<Serializer>(
                fsp, version, historyFile = persistenceFile, throwOnError = true, warmup = true, comparer = comparer)

        override __.PerfTester = tester :> _

        [<TestFixtureTearDown>]
        member __.Persist() =
            if persistResults then tester.PersistCurrentResults ()