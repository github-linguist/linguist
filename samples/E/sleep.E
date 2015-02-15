def sleep(milliseconds :int, nextThing) {
    stdout.println("Sleeping...")
    timer.whenPast(timer.now() + milliseconds, fn {
        stdout.println("Awake!")
        nextThing()
    })
}
