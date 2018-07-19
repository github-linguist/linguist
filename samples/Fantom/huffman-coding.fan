class Node
{
  Float probability := 0.0f
}

class Leaf : Node
{
  Int character

  new make (Int character, Float probability)
  {
    this.character = character
    this.probability = probability
  }
}

class Branch : Node
{
  Node left
  Node right

  new make (Node left, Node right)
  {
    this.left = left
    this.right = right
    probability = this.left.probability + this.right.probability
  }
}

class Huffman
{
  Node[] queue := [,]
  Str:Str table := [:]

  new make (Int[] items)
  {
    uniqueItems := items.dup.unique
    uniqueItems.each |Int item|
    {
      num := items.findAll { it == item }.size
      queue.add (Leaf(item, num.toFloat / items.size))
    }
    createTree
    createTable
  }

  Void createTree ()
  {
    while (queue.size > 1)
    {
      queue.sort |a,b| {a.probability <=> b.probability}
      node1 := queue.removeAt (0)
      node2 := queue.removeAt (0)
      queue.add (Branch (node1, node2))
    }
  }

  Void traverse (Node node, Str encoding)
  {
    if (node is Leaf)
    {
      table[(node as Leaf).character.toChar] = encoding
    }
    else // (node is Branch)
    {
      traverse ((node as Branch).left, encoding + "0")
      traverse ((node as Branch).right, encoding + "1")
    }
  }

  Void createTable ()
  {
    if (queue.size != 1) return // error!
    traverse (queue.first, "")
  }

  override Str toStr ()
  {
    result := "Huffman Encoding Table:\n"
    table.keys.sort.each |Str key|
    {
      result += "$key -> ${table[key]}\n"
    }
    return result
  }
}

class Main
{
  public static Void main ()
  {
    example := "this is an example for huffman encoding"
    huffman := Huffman (example.chars)
    echo ("From \"$example\"")
    echo (huffman)
  }
}
