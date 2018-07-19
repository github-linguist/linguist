class Main
{
  Int[] next_row (Int[] row)
  {
    new_row := [1]
    (row.size-1).times |i|
    {
      new_row.add (row[i] + row[i+1])
    }
    new_row.add (1)

    return new_row
  }

  Void print_pascal (Int n)  // no output for n <= 0
  {
    current_row := [1]
    n.times
    {
      echo (current_row.join(" "))
      current_row = next_row (current_row)
    }
  }

  Void main ()
  {
    print_pascal (10)
  }
}
