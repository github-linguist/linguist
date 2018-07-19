class Main
{
  // multiply two matrices (with no error checking)
  public static Int[][] multiply (Int[][] m1, Int[][] m2)
  {
    Int[][] result := [,]
    m1.each |Int[] row1|
    {
      Int[] row := [,]
      m2[0].size.times |Int colNumber|
      {
        Int value := 0
        m2.each |Int[] row2, Int index|
        {
          value += row1[index] * row2[colNumber]
        }
        row.add (value)
      }
     result.add (row)
    }
    return result
  }

  public static Void main ()
  {
    m1 := [[1,2,3],[4,5,6]]
    m2 := [[1,2],[3,4],[5,6]]

    echo ("${m1} times ${m2} = ${multiply(m1,m2)}")
  }
}
