import java.util.ArrayDeque

fun hailstone(n : Int) : ArrayDeque<Int> {
    val hails = when {
        n == 1 -> ArrayDeque<Int>()
        n % 2 == 0 -> hailstone(n / 2)
        else -> hailstone(3 * n + 1)
    }
    hails addFirst(n)
    return hails
}

fun main(args : Array<String>) {
    val hail27 = hailstone(27)
    fun showSeq(s : List<Int>) = s map {it.toString()} reduce {a, b -> a + ", " + b}
    System.out.println(
            "Hailstone sequence for 27 is " +
            showSeq(hail27 take(3)) + " ... " + showSeq(hail27 drop(hail27.size - 3)) +
            " with length ${hail27.size}."
    )

    var longestHail = hailstone(1)
    for (x in 1 .. 99999)
        longestHail = array(hailstone(x), longestHail) maxBy {it.size} ?: longestHail
    System.out.println(
            "${longestHail.getFirst()} is the number less than 100000 with " +
            "the longest sequence, having length ${longestHail.size}."
    )
}
