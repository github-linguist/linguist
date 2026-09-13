import "types"

contract Counter {
    author: "The Tolk Team"
    version: "1.0.0"
    description: "Simple counter. Example contract for learning Tolk"
    incomingMessages: AllowedMessage
    storage: Storage
}

type AllowedMessage = IncreaseCounter | ResetCounter

fun onInternalMessage(in: InMessage) {
    val msg = lazy AllowedMessage.fromSlice(in.body);

    match (msg) {
        IncreaseCounter => {
            var storage = lazy Storage.load();
            storage.counter += msg.increaseBy;
            storage.save();
        }
        ResetCounter => {
            var storage = lazy Storage.load();
            storage.counter = 0;
            storage.save();
        }
        else => {
            assert (in.body.isEmpty()) throw 0xFFFF;
        }
    }
}

get fun currentCounter(): int {
    val storage = lazy Storage.load();
    return storage.counter;
}
