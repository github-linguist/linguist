public class Node<T> {
   private T element;
   private Node<T> next, prev;

   public Node<T>(){
      next = prev = element = null;
   }

   public Node<T>(Node<T> n, Node<T> p, T elem){
      next = n;
      prev = p;
      element = elem;
   }

   public void setNext(Node<T> n){
      next = n;
   }

   public Node<T> getNext(){
      return next;
   }

   public void setElem(T elem){
      element = elem;
   }

   public T getElem(){
      return element;
   }

   public void setNext(Node<T> n){
      next = n;
   }

   public Node<T> setPrev(Node<T> p){
      prev = p;
   }

   public getPrev(){
      return prev;
   }
}
