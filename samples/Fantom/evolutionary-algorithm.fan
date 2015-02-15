class Main
{
  static const Str target := "METHINKS IT IS LIKE A WEASEL"
  static const Int C := 100     // size of population
  static const Float p := 0.1f  // chance any char is mutated

  // compute distance of str from target
  static Int fitness (Str str)
  {
    Int sum := 0
    str.each |Int c, Int index|
    {
      if (c != target[index]) sum += 1
    }
    return sum
  }

  // mutate given parent string
  static Str mutate (Str str)
  {
    Str result := ""
    str.size.times |Int index|
    {
      result += ((Float.random < p) ? randomChar() : str[index]).toChar
    }
    return result
  }

  // return a random char
  static Int randomChar ()
  {
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ "[Int.random(0..26)]
  }

  // make population by mutating parent and sorting by fitness
  static Str[] makePopulation (Str parent)
  {
    Str[] result := [,]
    C.times { result.add (mutate(parent)) }
    result.sort |Str a, Str b -> Int| { fitness(a) <=> fitness(b) }
    return result
  }

  public static Void main ()
  {
    Str parent := ""
    target.size.times { parent += randomChar().toChar }

    while (parent != target)
    {
      echo (parent)
      parent = makePopulation(parent).first
    }
    echo (parent)
  }
}
