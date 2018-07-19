import arsd.rpc;

struct S1 {
    int number;
    string name;
}

struct S2 {
    string name;
    int number;
}

interface ExampleNetworkFunctions {
    string sayHello(string name);
    int add(in int a, in int b) const pure nothrow;
    S2 structTest(S1);
    void die();
}

// The server must implement the interface.
class ExampleServer : ExampleNetworkFunctions {
    override string sayHello(string name) {
        return "Hello, " ~ name;
    }

    override int add(in int a, in int b) const pure nothrow {
        return a + b;
    }

    override S2 structTest(S1 a) {
        return S2(a.name, a.number);
    }

    override void die() {
        throw new Exception("death requested");
    }

    mixin NetworkServer!ExampleNetworkFunctions;
}

class Client {
    mixin NetworkClient!ExampleNetworkFunctions;
}

void main(in string[] args) {
    import std.stdio;

    if (args.length > 1) {
        auto client = new Client("localhost", 5005);
        // These work like the interface above, but instead of
        // returning the value, they take callbacks for success (where
        // the arg is the retval) and failure (the arg is the
        // exception).
        client.sayHello("whoa", (a) { writeln(a); }, null);
        client.add(1,2, (a){ writeln(a); }, null);
        client.add(10,20, (a){ writeln(a); }, null);
        client.structTest(S1(20, "cool!"),
                          (a){ writeln(a.name, " -- ", a.number); },
                          null);
        client.die(delegate(){ writeln("shouldn't happen"); },
                   delegate(a){ writeln(a); });
        client.eventLoop;
    } else {
        auto server = new ExampleServer(5005);
        server.eventLoop;
    }
}
