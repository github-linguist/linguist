import java.util.*;

class SameFringe
{
  public interface Node<T extends Comparable<? super T>>
  {
    Node<T> getLeft();
    Node<T> getRight();
    boolean isLeaf();
    T getData();
  }

  public static class SimpleNode<T extends Comparable<? super T>> implements Node<T>
  {
    private final T data;
    public SimpleNode<T> left;
    public SimpleNode<T> right;

    public SimpleNode(T data)
    {  this(data, null, null);  }

    public SimpleNode(T data, SimpleNode<T> left, SimpleNode<T> right)
    {
      this.data = data;
      this.left = left;
      this.right = right;
    }

    public Node<T> getLeft()
    {  return left;  }

    public Node<T> getRight()
    {  return right;  }

    public boolean isLeaf()
    {  return ((left == null) && (right == null));  }

    public T getData()
    {  return data;  }

    public SimpleNode<T> addToTree(T data)
    {
      int cmp = data.compareTo(this.data);
      if (cmp == 0)
        throw new IllegalArgumentException("Same data!");
      if (cmp < 0)
      {
        if (left == null)
          return (left = new SimpleNode<T>(data));
        return left.addToTree(data);
      }
      if (right == null)
        return (right = new SimpleNode<T>(data));
      return right.addToTree(data);
    }
  }

  public static <T extends Comparable<? super T>> boolean areLeavesSame(Node<T> node1, Node<T> node2)
  {
    Stack<Node<T>> stack1 = new Stack<Node<T>>();
    Stack<Node<T>> stack2 = new Stack<Node<T>>();
    stack1.push(node1);
    stack2.push(node2);
    // NOT using short-circuit operator
    while (((node1 = advanceToLeaf(stack1)) != null) & ((node2 = advanceToLeaf(stack2)) != null))
      if (!node1.getData().equals(node2.getData()))
        return false;
    // Return true if finished at same time
    return (node1 == null) && (node2 == null);
  }

  private static <T extends Comparable<? super T>> Node<T> advanceToLeaf(Stack<Node<T>> stack)
  {
    while (!stack.isEmpty())
    {
      Node<T> node = stack.pop();
      if (node.isLeaf())
        return node;
      Node<T> rightNode = node.getRight();
      if (rightNode != null)
        stack.push(rightNode);
      Node<T> leftNode = node.getLeft();
      if (leftNode != null)
        stack.push(leftNode);
    }
    return null;
  }

  public static void main(String[] args)
  {
    SimpleNode<Integer> headNode1 = new SimpleNode<Integer>(35, new SimpleNode<Integer>(25, new SimpleNode<Integer>(15, new SimpleNode<Integer>(10), new SimpleNode<Integer>(20)), new SimpleNode<Integer>(30)), new SimpleNode<Integer>(45, new SimpleNode<Integer>(40), new SimpleNode<Integer>(50)));
    SimpleNode<Integer> headNode2 = new SimpleNode<Integer>(24, new SimpleNode<Integer>(14, new SimpleNode<Integer>(10), new SimpleNode<Integer>(16, null, new SimpleNode<Integer>(20))), new SimpleNode<Integer>(34, new SimpleNode<Integer>(30), new SimpleNode<Integer>(42, new SimpleNode<Integer>(40), new SimpleNode<Integer>(56, new SimpleNode<Integer>(50), null))));
    SimpleNode<Integer> headNode3 = new SimpleNode<Integer>(24, new SimpleNode<Integer>(14, new SimpleNode<Integer>(10), new SimpleNode<Integer>(16, null, new SimpleNode<Integer>(20))), new SimpleNode<Integer>(34, new SimpleNode<Integer>(30), new SimpleNode<Integer>(42, new SimpleNode<Integer>(40), new SimpleNode<Integer>(50, null, new SimpleNode<Integer>(56)))));
    System.out.print("Leaves for set 1: ");
    simpleWalk(headNode1);
    System.out.println();
    System.out.print("Leaves for set 2: ");
    simpleWalk(headNode2);
    System.out.println();
    System.out.print("Leaves for set 3: ");
    simpleWalk(headNode3);
    System.out.println();
    System.out.println("areLeavesSame(1, 2)? " + areLeavesSame(headNode1, headNode2));
    System.out.println("areLeavesSame(2, 3)? " + areLeavesSame(headNode2, headNode3));
  }

  public static void simpleWalk(Node<Integer> node)
  {
    if (node.isLeaf())
      System.out.print(node.getData() + " ");
    else
    {
      Node<Integer> left = node.getLeft();
      if (left != null)
        simpleWalk(left);
      Node<Integer> right = node.getRight();
      if (right != null)
        simpleWalk(right);
    }
  }
}
