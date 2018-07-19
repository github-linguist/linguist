// define a class to contain the two fields
// accessors to get/set the field values are automatically generated
class Point
{
  Int x
  Int y
}

class Main
{
  public static Void main ()
  {
    // empty constructor, so x,y set to 0
    point1 := Point()
    // constructor uses with-block, to initialise values
    point2 := Point { x = 1; y = 2}
    echo ("Point 1 = (" + point1.x + ", " + point1.y + ")")
    echo ("Point 2 = (" + point2.x + ", " + point2.y + ")")
  }
}
