def isSorted(l: List[Int]) = l.iterator sliding 2 forall (s => s.head < s.last)
def bogosort(l: List[Int]): List[Int] = if (isSorted(l)) l else bogosort(scala.util.Random.shuffle(l))
