class Automaton
{
  static Int[] evolve (Int[] array)
  {
    return array.map |Int x, Int i -> Int|
    {
      if (i == 0)
        return ( (x + array[1] == 2) ? 1 : 0)
      else if (i == array.size-1)
        return ( (x + array[-2] == 2) ? 1 : 0)
      else if (x + array[i-1] + array[i+1] == 2)
        return 1
      else
        return 0
    }
  }

  public static Void main ()
  {
    Int[] array := [0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0]
    echo (array.join(""))
    Int[] newArray := evolve(array)
    while (newArray != array)
    {
      echo (newArray.join(""))
      array = newArray
      newArray = evolve(array)
    }
  }
}
