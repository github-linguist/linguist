import java.util.LinkedList;

public static void main(){
    LinkedList<String> LL = new LinkedList<String>();
    traverse(LL.iterator());
    traverse(LL.descendingIterator());
}

private static void traverse(Iterator<String> iter){
    while(iter.hasNext()){
        iter.next();
    }
}
