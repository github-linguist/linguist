def example {
    to foo() { println("this is foo") }
    to bar() { println("this is bar") }
    match [verb, args] {
        println(`got unrecognized message $verb`)
        if (args.size() > 0) {
            println(`it had arguments: $args`)
        }
    }
}
