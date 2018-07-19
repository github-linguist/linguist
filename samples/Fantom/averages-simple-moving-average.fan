class MovingAverage
{
  Int period
  Int[] stream

  new make (Int period)
  {
    this.period = period
    stream = [,]
  }

  // add number to end of stream and remove numbers from start if
  // stream is larger than period
  public Void addNumber (Int number)
  {
    stream.add (number)
    while (stream.size > period)
    {
      stream.removeAt (0)
    }
  }

  // compute average of numbers in stream
  public Float average ()
  {
    if (stream.isEmpty)
      return 0.0f
    else
      1.0f * (Int)(stream.reduce(0, |a,b| { (Int) a + b })) / stream.size
  }
}

class Main
{
  public static Void main ()
  { // test by adding random numbers and printing average after each number
    av := MovingAverage (5)

    10.times |i|
    {
      echo ("After $i numbers list is ${av.stream} average is ${av.average}")
      av.addNumber (Int.random(0..100))
    }
  }
}
