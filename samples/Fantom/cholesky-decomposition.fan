**
** Cholesky decomposition
**

class Main
{
  // create an array of Floats, initialised to 0.0
  Float[][] makeArray (Int i, Int j)
  {
    Float[][] result := [,]
    i.times { result.add ([,]) }
    i.times |Int x|
    {
      j.times
      {
        result[x].add(0f)
      }
    }
    return result
  }

  // perform the Cholesky decomposition
  Float[][] cholesky (Float[][] array)
  {
    m := array.size
    Float[][] l := makeArray (m, m)
    m.times |Int i|
    {
      (i+1).times |Int k|
      {
        Float sum := (0..<k).toList.reduce (0f) |Float a, Int j -> Float|
        {
          a + l[i][j] * l[k][j]
        }
        if (i == k)
          l[i][k] = (array[i][i]-sum).sqrt
        else
          l[i][k] = (1.0f / l[k][k]) * (array[i][k] - sum)
      }
    }
    return l
  }

  Void runTest (Float[][] array)
  {
    echo (array)
    echo (cholesky (array))
  }

  Void main ()
  {
    runTest ([[25f,15f,-5f],[15f,18f,0f],[-5f,0f,11f]])
    runTest ([[18f,22f,54f,42f],[22f,70f,86f,62f],[54f,86f,174f,134f],[42f,62f,134f,106f]])
  }
}
