public class Queue<E>{
    Node<E> head = null, tail = null;

    static class Node<E>{
        E value;
        Node<E> next;

        Node(E value, Node<E> next){
            this.value= value;
            this.next= next;
        }

    }

    public Queue(){
    }

    public void enqueue(E value){ //standard queue name for "push"
        Node<E> newNode= new Node<E>(value, null);
        if(empty()){
            head= newNode;
        }else{
            tail.next = newNode;
        }
        tail= newNode;
    }

    public E dequeue() throws java.util.NoSuchElementException{//standard queue name for "pop"
        if(empty()){
            throw new java.util.NoSuchElementException("No more elements.");
        }
        E retVal= head.value;
        head= head.next;
        return retVal;
    }

    public boolean empty(){
        return head == null;
    }
}
