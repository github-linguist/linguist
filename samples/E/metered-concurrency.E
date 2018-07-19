def makeSemaphore(maximum :(int > 0)) {
    var current := 0
    def waiters := <elib:vat.makeQueue>()
    def notify() {
        while (current < maximum && waiters.hasMoreElements()) {
            current += 1
            waiters.optDequeue().resolve(def released)
            when (released) -> {
                current -= 1
                notify()
            }
        }
    }
    def semaphore {
        to acquire() {
            waiters.enqueue(def response)
            notify()
            return response
        }
        to count() { return current }
    }
    return semaphore
}

def work(label, interval, semaphore, timer, println) {
    when (def releaser := semaphore <- acquire()) -> {
        println(`$label: I have acquired the lock.`)
        releaser.resolve(
            timer.whenPast(timer.now() + interval, fn {
                println(`$label: I will have released the lock.`)
            })
        )
    }
}

def semaphore := makeSemaphore(3)
for i in 1..5 {
    work(i, 2000, semaphore, timer, println)
}
