class LinkedList(T)
{
 Node!(T) head, tail;

 /** Iterate in the forward direction. */
 int opApply (int delegate(uint, Node!(T)) dg)
 {
  uint i = 0;
  auto link = head;
  int result = 0;
  while (link)
  {
   result = dg (i, link);
   if (result) return result;
   i++;
   link = link.next;
  }
  return result;
 }

 static LinkedList!(T) fromArray (T[] array)
 {
  Node!(T) link = null;
  auto head = link;
  auto self = new LinkedList!(T);
  foreach (elem; array)
  {
   link = new Node!(T)(null, link, elem, self);
   if (!head)
    head = link;
  }
  return self;
 }
}

class Node(T)
{
 Node!(T) next;
 Node!(T) previous;
 LinkedList!(T) parent;
 T value;

 this (Node!(T) next, Node!(T) previous, T value, LinkedList!(T) parent)
 in
 {
  assert (parent !is null);
 }
 body
 {
  this.next = next;
  if (next)
   next.previous = this;
  if (previous)
   previous.next = this;
  this.previous = previous;
  this.value = value;
  this.parent = parent;

  if (parent.head == next)
   parent.head = this;
  if (parent.tail == previous)
   parent.tail = this;
 }

 /** Insert an element after this one. */
 void insertAfter (T value)
 {
  new Node!(T)(this, next, value, parent);
 }

 /** Insert an element before this one. */
 void insertBefore (T value)
 {
  new Node!(T)(previous, this, value, parent);
 }

 /** Remove the current node from the list. */
 void remove ()
 {
  if (next)
   next.previous = previous;
  if (previous)
   previous.next = next;
  if (parent.tail == this)
   parent.tail = previous;
  if (parent.head == this)
   parent.head = next;
 }
}

void main ()
{
 char[][] sample = ["was", "it", "a", "cat", "I", "saw"];
 auto list = LinkedList!(char[]).fromArray (sample);
 for (auto elem = list.head; elem; elem = elem.next)
 {
  writef ("%s ", elem.value);
  if (elem.value == "it") elem.insertAfter("really");
 }
 writeln;
 for (auto elem = list.tail; elem; elem = elem.previous)
 {
  writef ("%s ", elem.value);
 }
 writeln;
}
