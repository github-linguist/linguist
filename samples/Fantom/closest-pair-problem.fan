class Point
{
  Float x
  Float y

  // create a random point
  new make (Float x := Float.random * 10, Float y := Float.random * 10)
  {
    this.x = x
    this.y = y
  }

  Float distance (Point p)
  {
    ((x-p.x)*(x-p.x) + (y-p.y)*(y-p.y)).sqrt
  }

  override Str toStr () { "($x, $y)" }
}

class Main
{
  // use brute force approach
  static Point[] findClosestPair1 (Point[] points)
  {
    if (points.size < 2) return points  // list too small
    Point[] closestPair := [points[0], points[1]]
    Float closestDistance := points[0].distance(points[1])

    (1..<points.size).each |Int i|
    {
      ((i+1)..<points.size).each |Int j|
      {
        Float trydistance := points[i].distance(points[j])
        if (trydistance < closestDistance)
        {
          closestPair = [points[i], points[j]]
          closestDistance = trydistance
        }
      }
    }

    return closestPair
  }

  // use recursive divide-and-conquer approach
  static Point[] findClosestPair2 (Point[] points)
  {
    if (points.size <= 3) return findClosestPair1(points)
    points.sort |Point a, Point b -> Int| { a.x <=> b.x }
    bestLeft := findClosestPair2 (points[0..(points.size/2)])
    bestRight := findClosestPair2 (points[(points.size/2)..-1])

    Float minDistance
    Point[] closePoints := [,]
    if (bestLeft[0].distance(bestLeft[1]) < bestRight[0].distance(bestRight[1]))
    {
      minDistance = bestLeft[0].distance(bestLeft[1])
      closePoints = bestLeft
    }
    else
    {
      minDistance = bestRight[0].distance(bestRight[1])
      closePoints = bestRight
    }
    yPoints := points.findAll |Point p -> Bool|
    {
      (points.last.x - p.x).abs < minDistance
    }.sort |Point a, Point b -> Int| { a.y <=> b.y }

    closestPair := [,]
    closestDist := Float.posInf

    for (Int i := 0; i < yPoints.size - 1; ++i)
    {
      for (Int j := (i+1); j < yPoints.size; ++j)
      {
        if ((yPoints[j].y - yPoints[i].y) >= minDistance)
        {
          break
        }
        else
        {
          dist := yPoints[i].distance (yPoints[j])
          if (dist < closestDist)
          {
            closestDist = dist
            closestPair = [yPoints[i], yPoints[j]]
          }
        }
      }
    }
    if (closestDist < minDistance)
      return closestPair
    else
      return closePoints
  }

  public static Void main (Str[] args)
  {
    Int numPoints := 10 // default value, in case a number not given on command line
    if ((args.size > 0) && (args[0].toInt(10, false) != null))
    {
      numPoints = args[0].toInt(10, false)
    }

    Point[] points := [,]
    numPoints.times { points.add (Point()) }

    Int t1 := Duration.now.toMillis
    echo (findClosestPair1(points.dup))
    Int t2 := Duration.now.toMillis
    echo ("Time taken: ${(t2-t1)}ms")
    echo (findClosestPair2(points.dup))
    Int t3 := Duration.now.toMillis
    echo ("Time taken: ${(t3-t2)}ms")
  }
}
