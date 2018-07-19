class Main
{
  // Create and return a function which generates mth powers when called
  |->Int| make_generator (Int m)
  {
    current := 0
    return |->Int|
    {
      current += 1
      return (current-1).pow (m)
    }
  }

  |->Int| squares_without_cubes ()
  {
    squares := make_generator (2)
    cubes := make_generator (3)
    c := cubes.call
    return |->Int|
    {
      while (true)
      {
        s := squares.call
        while (c < s) { c = cubes.call }
        if (c != s) return s
      }
      return 0
    }
  }

  Void main ()
  {
    swc := squares_without_cubes ()
    20.times { swc.call } // drop 20 values
    10.times // display the next 10
    {
      echo (swc.call)
    }
  }
}
