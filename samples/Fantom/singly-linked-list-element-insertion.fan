class Node
{
  const Int value
  Node? successor // can be null, for end of series

  new make (Int value, Node? successor := null)
  {
    this.value = value
    this.successor = successor
  }

  // insert method for this problem
  public Void insert (Node newNode)
  {
    newNode.successor = this.successor
    this.successor = newNode
  }
}

// simple class to test putting 'c' between 'a' and 'b'
class Main
{
  public static Void main ()
  {
    c := Node (2)
    b := Node (3)
    a := Node (1, b)
    a.insert (c)

    echo (a.value)
    echo (a.successor.value)
    echo (a.successor.successor.value)
  }
}
