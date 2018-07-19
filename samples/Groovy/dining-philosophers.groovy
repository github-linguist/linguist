import groovy.transform.Canonical

import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock

@Canonical
class Fork {
    String name
    Lock lock = new ReentrantLock()

    void pickUp(String philosopher) {
        lock.lock()
        println "  $philosopher picked up $name"
    }

    void putDown(String philosopher) {
        lock.unlock()
        println "  $philosopher put down $name"
    }
}

@Canonical
class Philosopher extends Thread {
    Fork f1
    Fork f2

    @Override
    void run() {
        def random = new Random()
        (1..20).each { bite ->
            println "$name is hungry"
            f1.pickUp name
            f2.pickUp name
            println "$name is eating bite $bite"
            Thread.sleep random.nextInt(300) + 100
            f2.putDown name
            f1.putDown name
        }
    }
}

void diningPhilosophers(names) {
    def forks = (1..names.size()).collect { new Fork(name: "Fork $it") }
    def philosophers = []
    names.eachWithIndex{ n, i ->
        def (i1, i2) = [i, (i + 1) % 5]
        if (i2 < i1) (i1, i2) = [i2, i]

        def p = new Philosopher(name: n, f1: forks[i1], f2: forks[i2])
        p.start()
        philosophers << p
    }
    philosophers.each { it.join() }
}

diningPhilosophers(['Aristotle', 'Kant', 'Spinoza', 'Marx', 'Russell'])
