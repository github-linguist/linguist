import java.math.BigInteger;
import java.util.PriorityQueue;

val Three = BigInteger.valueOf(3)
val Five = BigInteger.valueOf(5)

fun updateFrontier(x : BigInteger, pq : PriorityQueue<BigInteger>) {
    pq add(x shiftLeft(1))
    pq add(x multiply(Three))
    pq add(x multiply(Five))
}

fun hamming(n : Int) : BigInteger {
    val frontier = PriorityQueue<BigInteger>()
    updateFrontier(BigInteger.ONE, frontier)
    var lowest = BigInteger.ONE
    for (i in 1 .. n-1) {
        lowest = frontier.poll() ?: lowest
        while (frontier.peek() equals(lowest))
            frontier.poll()
        updateFrontier(lowest, frontier)
    }
    return lowest
}

fun main(args : Array<String>) {
    System.out print("Hamming(1 .. 20) =")
    for (i in 1 .. 20)
        System.out print(" ${hamming(i)}")
    System.out println("\nHamming(1691) = ${hamming(1691)}")
    System.out println("Hamming(1000000) = ${hamming(1000000)}")
}
