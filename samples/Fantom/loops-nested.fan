class Main
{
  public static Void main ()
  {
    rows := 10
    cols := 10
    // create and fill an array of given size with random numbers
    Int[][] array := [,]
    rows.times
    {
      row := [,]
      cols.times { row.add(Int.random(1..20)) }
      array.add (row)
    }
    // now do the search
    try
    {
      for (i := 0; i < rows; i++)
      {
        for (j := 0; j < cols; j++)
        {
          echo ("now at ($i, $j) which is ${array[i][j]}")
          if (array[i][j] == 20) throw (Err("found it"))
        }
      }
    }
    catch (Err e)
    {
      echo (e.msg)
      return // and finish
    }
    echo ("No 20")
  }
}
