import java.util.Comparator
import java.util.ArrayList

fun <T> quickSort(a : List<T>, c : Comparator<T>) : ArrayList<T> {
    return if (a.size == 0) ArrayList(a)
    else {
        val boxes = Array<ArrayList<T>>(3, {ArrayList<T>()})
        fun normalise(i : Int) = i / Math.max(1, Math.abs(i))
        a forEach {boxes[normalise(c.compare(it, a[0])) + 1] add(it)}
        array(0, 2) forEach {boxes[it] = quickSort(boxes[it], c)}
        boxes.flatMapTo(ArrayList<T>()) {it}
    }
}
