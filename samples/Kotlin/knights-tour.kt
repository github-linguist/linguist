import java.util.ArrayList

class Square(val x : Int, val y : Int) {
    fun equals(s : Square) : Boolean = s.x == x && s.y == y
}

class Pair<T>(val a : T, val b : T)

val board = Array<Square>(8 * 8, {Square(it / 8 + 1, it % 8 + 1)})
val axisMoves = array(1, 2, -1, -2)

fun allPairs<T>(a : Array<T>) = a flatMap {i -> a map {j -> Pair(i, j)}}

fun knightMoves(s : Square) : List<Square> {
    val moves = allPairs(axisMoves) filter {Math.abs(it.a) != Math.abs(it.b)}
    fun onBoard(s : Square) = board.any {it equals s}
    return moves map {Square(s.x + it.a, s.y + it.b)} filter {onBoard(it)}
}

fun knightTour(moves : List<Square>) : List<Square> {
    fun findMoves(s : Square) = knightMoves(s) filterNot {m -> moves any {it equals m}}
    val newSquare = findMoves(moves.last()) minBy {findMoves(it).size}
    return if (newSquare == null) moves else knightTour(moves + newSquare)
}

fun knightTourFrom(start : Square) = knightTour(array(start).toList())

fun main(args : Array<String>) {
    var col = 0
    for (move in knightTourFrom(Square(1, 1))) {
        System.out.print("${move.x},${move.y}")
        System.out.print(if (col == 7) "\n" else " ")
        col = (col + 1) % 8
    }
}
