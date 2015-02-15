class Defn // to hold the three numbers from a 'row' in the table
{
  Float low
  Float high
  Float value
  new make (Float low, Float high, Float value)
  {
    this.low = low
    this.high = high
    this.value = value
  }
}

class PriceConverter
{
  Defn[] defns := [,]
  new make (Str table) // process given table and store numbers from each row in a defn
  {
    table.split('\n').each |Str line|
    {
      data := line.split
      defns.add (Defn(Float.fromStr(data[1]), Float.fromStr(data[3]), Float.fromStr(data[5])))
    }
  }

  public Float convert (Float price) // convert by looking through list of defns
  {
    Float result := price
    defns.each |Defn defn|
    {
      if (price >= defn.low && price < defn.high)
        result = defn.value
    }
    return result
  }
}

class Main
{
  public static Void main ()
  {
    table := ">=  0.00  <  0.06  :=  0.10
              >=  0.06  <  0.11  :=  0.18
              >=  0.11  <  0.16  :=  0.26
              >=  0.16  <  0.21  :=  0.32
              >=  0.21  <  0.26  :=  0.38
              >=  0.26  <  0.31  :=  0.44
              >=  0.31  <  0.36  :=  0.50
              >=  0.36  <  0.41  :=  0.54
              >=  0.41  <  0.46  :=  0.58
              >=  0.46  <  0.51  :=  0.62
              >=  0.51  <  0.56  :=  0.66
              >=  0.56  <  0.61  :=  0.70
              >=  0.61  <  0.66  :=  0.74
              >=  0.66  <  0.71  :=  0.78
              >=  0.71  <  0.76  :=  0.82
              >=  0.76  <  0.81  :=  0.86
              >=  0.81  <  0.86  :=  0.90
              >=  0.86  <  0.91  :=  0.94
              >=  0.91  <  0.96  :=  0.98
              >=  0.96  <  1.01  :=  1.00"
    converter := PriceConverter (table)
    10.times  // simple test with random values
    {
      price := (0..100).random.toFloat / 100
      echo ("$price -> ${converter.convert (price)}")
    }
  }
}
