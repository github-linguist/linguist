using fwt
using gfx

class Main
{
  public static Void main ()
  {
    particles := Particles (300, 200)
    1000.times { particles.addParticle } // add 1000 particles
    Window // open up a display for the final tree
    {
      title = "Brownian Tree"
      EdgePane
      {
        center = ScrollPane { content = ParticleCanvas(particles) }
      },
    }.open
  }
}

class Particles
{
  Bool[][] image
  Int height
  Int width

  new make (Int height, Int width)
  {
    this.height = height
    this.width = width
    // set up initial image as an array of booleans with one set cell
    image = [,]
    width.times |w|
    {
      row := [,]
      height.times { row.add (false) }
      image.add (row)
    }
    image[Int.random(0..<width)][Int.random(0..<height)] = true
  }

  Bool get (Int w, Int h) { return image[w][h] }

  Void addParticle ()
  {
    x := Int.random(0..<width)
    y := Int.random(0..<height)

    Int dx := 0
    Int dy := 0
    while (!image[x][y]) // loop until hit existing part of the tree
    {
      dx = [-1,0,1].random
      dy = [-1,0,1].random

      if ((0..<width).contains(x + dx))
        x += dx
      else // did not change x, so set dx = 0
        dx = 0
      if ((0..<height).contains(y + dy))
        y += dy
      else
        dy = 0
    }

    // put x,y back to just before move onto existing part of tree
    x -= dx
    y -= dy

    image[x][y] = true
  }
}

class ParticleCanvas : Canvas
{
  Particles particles

  new make (Particles particles) { this.particles = particles }

  // provides canvas size for parent scrollpane
  override Size prefSize(Hints hints := Hints.defVal)
  {
    Size(particles.width, particles.height)
  }

  // repaint the display
  override Void onPaint (Graphics g)
  {
    g.brush = Color.black
    g.fillRect(0, 0, size.w, size.h)
    g.brush = Color.green
    particles.width.times |w|
    {
      particles.height.times |h|
      {
        if (particles.get(w, h)) // draw a 1x1 square for each set particle
          g.fillRect (w, h, 1, 1)
      }
    }
  }
}
