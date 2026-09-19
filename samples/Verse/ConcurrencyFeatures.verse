using. /Verse.org/Concurrency
using. /Verse.org/Simulation

ConcurrencyFeatures<public> := module {

    ArrayRace<public>(Array:[]t, HandlerFunction(Element:t)<suspends>:u where t:type, u:type)<suspends>:u = {
        if (
            Array.Length > 1
            MidPoint := Floor(Array.Length / 2)
            LeftArray := Array.Slice[0, MidPoint]
            RightArray := Array.Slice[MidPoint]
        ) {
            race {
                ArrayRace(LeftArray, HandlerFunction)
                ArrayRace(RightArray, HandlerFunction)
            }
        } else if (FirstElement := Array[0]) {
            HandlerFunction(FirstElement)
        } else {
            Sleep(Inf)
            Err("Unreachable")
        }
    }

    ArrayRush<public>(Array:[]t, HandlerFunction(Element:t)<suspends>:u where t:type, u:type)<suspends>:u = {
        if (
            Array.Length > 1
            MidPoint := Floor(Array.Length / 2)
            LeftArray := Array.Slice[0, MidPoint]
            RightArray := Array.Slice[MidPoint]
        ) {
            rush {
                ArrayRush(LeftArray, HandlerFunction)
                ArrayRush(RightArray, HandlerFunction)
            }
        } else if (FirstElement := Array[0]) {
            HandlerFunction(FirstElement)
        } else {
            Sleep(Inf)
            Err("Unreachable")
        }
    }

    ArraySync<public>(Array:[]t, HandlerFunction(Element:t)<suspends>:u where t:type, u:type)<suspends>:[]u = {
        if (
            Array.Length > 1
            MidPoint := Floor(Array.Length / 2)
            LeftArray := Array.Slice[0, MidPoint]
            RightArray := Array.Slice[MidPoint]
        ) {
            SyncResult := sync {
                ArraySync(LeftArray, HandlerFunction)
                ArraySync(RightArray, HandlerFunction)
            }

            SyncResult(0) + SyncResult(1)

        } else if (FirstElement := Array[0]) {
            array{HandlerFunction(FirstElement)}
        } else {
            array{}
        }
    }
}