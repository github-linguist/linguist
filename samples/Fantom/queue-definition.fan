class Queue
{
  List queue := [,]

  public Void push (Obj obj)
  {
    queue.add (obj)  // add to right of list
  }

  public Obj pop ()
  {
    if (queue.isEmpty)
      throw (Err("queue is empty"))
    else
    {
      return queue.removeAt(0) // removes left-most item
    }
  }

  public Bool isEmpty ()
  {
    queue.isEmpty
  }
}
