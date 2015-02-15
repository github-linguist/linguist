class World
{
  Int height
  Int width
  Bool[] state

  new make (Int height, Int width)
  {
    this.height = height
    this.width = width
    state = List(Bool#, height * width)
    (height*width).times { state.add (false) }
  }

  Bool inWorld (Int x, Int y)
  {
    x >= 0 && x < width && y >= 0 && y < height
  }

  Void show ()
  {
    height.times |h|
    {
      width.times |w|
      {
        Env.cur.out.writeChar (state[w*width+h] ? '#' : '.')
      }
      Env.cur.out.writeChar ('\n')
    }
  }

  Void flip (Int x, Int y)
  {
    state[x*width + y] = !state[x*width + y]
  }

  Bool stateOf (Int x, Int y)
  {
    state[x*width + y]
  }
}

enum class Direction
{
  up (0, -1),
  down (0, 1),
  left (-1, 0),
  right (1, 0)

  private new make (Int deltaX, Int deltaY)
  {
    this.deltaX = deltaX
    this.deltaY = deltaY
  }

  Direction rotateLeft ()
  {
    if (this == up) return left
    if (this == down) return right
    if (this == left) return down
    // if (this == right)
    return up
  }

  Direction rotateRight ()
  {
    if (this == up) return right
    if (this == down) return left
    if (this == left) return up
    // if (this == right)
    return down
  }

  const Int deltaX
  const Int deltaY
}

class Ant
{
  World world
  Int currX
  Int currY
  Direction direction

  new make (World world, Int x, Int y)
  {
    this.world = world
    currX = x
    currY = y
    direction = Direction.up
  }

  Bool inWorld ()
  {
    world.inWorld (currX, currY)
  }

  // the ant movement rules
  Void move ()
  {
    if (world.stateOf (currX, currY))
    {
      direction = direction.rotateLeft
    }
    else
    {
      direction = direction.rotateRight
    }
    world.flip (currX, currY)
    currX += direction.deltaX
    currY += direction.deltaY
  }
}

class Main
{
  Void main ()
  {
    world := World (100, 100)
    ant := Ant (world, 50, 50)
    numIterations := 0
    while (ant.inWorld)
    {
      ant.move
      numIterations += 1
    }
    world.show
    echo ("Finished in $numIterations iterations")
  }
}
