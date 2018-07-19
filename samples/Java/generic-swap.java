class Pair<T> {
    T first;
    T second;
}
public static <T> void swap(Pair<T> p) {
   T temp = p.first;
   p.first = p.second;
   p.second = temp;
}
