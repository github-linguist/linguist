class Node
{
  const Int value  // keep value fixed
  Node? successor  // allow successor to change, also, can be 'null', for end of list

  new make (Int value, Node? successor := null)
  {
    this.value = value
    this.successor = successor
  }
}
