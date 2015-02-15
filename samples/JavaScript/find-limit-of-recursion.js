function recurse(depth)
{
 try
 {
  return recurse(depth + 1);
 }
 catch(ex)
 {
  return depth;
 }
}

var maxRecursion = recurse(1);
document.write("Recursion depth on this system is " + maxRecursion);
