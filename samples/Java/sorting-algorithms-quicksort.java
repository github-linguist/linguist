public static <E extends Comparable<? super E>> List<E> quickSort(List<E> arr) {
if (!arr.isEmpty()) {
    E pivot = arr.get(0); //This pivot can change to get faster results


    List<E> less = new LinkedList<E>();
    List<E> pivotList = new LinkedList<E>();
    List<E> more = new LinkedList<E>();

    // Partition
    for (E i: arr) {
        if (i.compareTo(pivot) < 0)
            less.add(i);
        else if (i.compareTo(pivot) > 0)
            more.add(i);
        else
            pivotList.add(i);
    }

    // Recursively sort sublists
    less = quickSort(less);
    more = quickSort(more);

    // Concatenate results
    less.addAll(pivotList);
    less.addAll(more);
    return less;
 }
return arr;

}
