#!/usr/bin/env rune
pragma.syntax("0.9")

def pi := (-1.0).acos()
def makeEPainter := <unsafe:com.zooko.tray.makeEPainter>
def makeLamportSlot := <import:org.erights.e.elib.slot.makeLamportSlot>
def whenever := <import:org.erights.e.elib.slot.whenever>
def colors := <import:java.awt.makeColor>

# --------------------------------------------------------------
# --- Definitions

def makePendulumSim(length_m :float64,
                    gravity_mps2 :float64,
                    initialAngle_rad :float64,
                    timestep_ms :int) {
  var velocity := 0
  def &angle := makeLamportSlot(initialAngle_rad)
  def k := -gravity_mps2/length_m
  def timestep_s := timestep_ms / 1000
  def clock := timer.every(timestep_ms, fn _ {
    def acceleration := k * angle.sin()
    velocity += acceleration * timestep_s
    angle    += velocity     * timestep_s
  })
  return [clock, &angle]
}

def makeDisplayComponent(&angle) {
  def c
  def updater := whenever([&angle], fn { c.repaint() })

  bind c := makeEPainter(def paintCallback {
    to paintComponent(g) {
      try {
        def originX := c.getWidth() // 2
        def originY := c.getHeight() // 2
        def pendRadius := (originX.min(originY) * 0.95).round()
        def ballRadius := (originX.min(originY) * 0.04).round()
        def ballX := (originX + angle.sin() * pendRadius).round()
        def ballY := (originY + angle.cos() * pendRadius).round()

        g.setColor(colors.getWhite())
        g.fillRect(0, 0, c.getWidth(), c.getHeight())
        g.setColor(colors.getBlack())

        g.fillOval(originX - 2, originY - 2, 4, 4)
        g.drawLine(originX, originY, ballX, ballY)
        g.fillOval(ballX - ballRadius, ballY - ballRadius, ballRadius * 2, ballRadius * 2)

        updater[] # provoke interest provided that we did get drawn (window not closed)
      } catch p {
        stderr.println(`In paint callback: $p${p.eStack()}`)
      }
    }
  })

  c.setPreferredSize(<awt:makeDimension>(300, 300))
  return c
}

# --------------------------------------------------------------
# --- Application setup

def [clock, &angle] := makePendulumSim(1, 9.80665, pi*99/100, 10)

# Initialize AWT, move to AWT event thread
when (currentVat.morphInto("awt")) -> {

  # Create the window
  def frame := <unsafe:javax.swing.makeJFrame>("Pendulum")
  frame.setContentPane(def display := makeDisplayComponent(&angle))
  frame.addWindowListener(def mainWindowListener {
    to windowClosing(_) {
      clock.stop()
      interp.continueAtTop()
    }
    match _ {}
  })
  frame.setLocation(50, 50)
  frame.pack()

  # Start and become visible
  frame.show()
  clock.start()
}

interp.blockAtTop()
