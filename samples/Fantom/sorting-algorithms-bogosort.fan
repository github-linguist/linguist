class Main
{
  Bool in_order (Int[] items)
  {
    (0..<(items.size-1)).toList.all |Int i -> Bool|
    {
      items[i] <= items[i+1]
    }
  }

  Int[] bogosort (Int[] items)
  {
    while (!in_order(items))
    {
      items.shuffle
    }
    return items
  }

  Void main ()
  {
    // example
    echo ("Sorting [3,4,2,1] gives " + bogosort ([3,4,2,1]))
  }
}
