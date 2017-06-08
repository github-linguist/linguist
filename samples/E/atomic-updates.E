#!/usr/bin/env rune
pragma.syntax("0.9")

def pi := (-1.0).acos()
def makeEPainter := <unsafe:com.zooko.tray.makeEPainter>
def colors := <awt:makeColor>

# --------------------------------------------------------------
# --- Definitions

/** Execute 'task' repeatedly as long 'indicator' is unresolved. */
def doWhileUnresolved(indicator, task) {
  def loop() {
    if (!Ref.isResolved(indicator)) {
      task()
      loop <- ()
    }
  }
  loop <- ()
}

/** The data structure specified for the task. */
def makeBuckets(size) {
    def values := ([100] * size).diverge() # storage
    def buckets {
        to size() :int { return size }
        /** get current quantity in bucket 'i' */
        to get(i :int) { return values[i] }
        /** transfer 'amount' units, as much as possible, from bucket 'i' to bucket 'j'
            or vice versa if 'amount' is negative */
        to transfer(i :int, j :int, amount :int) {
            def amountLim := amount.min(values[i]).max(-(values[j]))
            values[i] -= amountLim
            values[j] += amountLim
        }
    }
    return buckets
}

/** A view of the current state of the buckets. */
def makeDisplayComponent(buckets) {
  def c := makeEPainter(def paintCallback {
    to paintComponent(g) {
      def pixelsW := c.getWidth()
      def pixelsH := c.getHeight()
      def bucketsW := buckets.size()

      g.setColor(colors.getWhite())
      g.fillRect(0, 0, pixelsW, pixelsH)

      g.setColor(colors.getDarkGray())
      var sum := 0
      for i in 0..!bucketsW {
        sum += def value := buckets[i]
        def x0 := (i       * pixelsW / bucketsW).floor()
        def x1 := ((i + 1) * pixelsW / bucketsW).floor()
        g.fillRect(x0 + 1, pixelsH - value,
                   x1 - x0 - 1, value)
      }

      g.setColor(colors.getBlack())
      g."drawString(String, int, int)"(`Total: $sum`, 2, 20)
    }
  })
  c.setPreferredSize(<awt:makeDimension>(500, 300))
  return c
}

# --------------------------------------------------------------
# --- Application setup

def buckets := makeBuckets(100)
def done # Promise indicating when the window is closed

# Create the window
def frame := <unsafe:javax.swing.makeJFrame>("Atomic transfers")
frame.setContentPane(def display := makeDisplayComponent(buckets))
frame.addWindowListener(def mainWindowListener {
  to windowClosing(event) :void {
    bind done := null
  }
  match _ {}
})
frame.setLocation(50, 50)
frame.pack()

# --------------------------------------------------------------
# --- Tasks

# Neatens up buckets
var ni := 0
doWhileUnresolved(done, fn {
  def i := ni
  def j := (ni + 1) %% buckets.size()
  buckets.transfer(i, j, (buckets[i] - buckets[j]) // 4)
  ni := j
})

# Messes up buckets
var mi := 0
doWhileUnresolved(done, fn {
    def i := (mi + entropy.nextInt(3)) %% buckets.size()
    def j := (i + entropy.nextInt(3)) %% buckets.size() #entropy.nextInt(buckets.size())
    buckets.transfer(i, j, (buckets[i] / pi).floor())
    mi := j
})

# Updates display at fixed 10 Hz
# (Note: tries to catch up; on slow systems slow this down or it will starve the other tasks)
def clock := timer.every(100, def _(_) {
  if (Ref.isResolved(done)) {
    clock.stop()
  } else {
    display.repaint()
  }
})
clock.start()

# --------------------------------------------------------------
# --- All ready, go visible and wait

frame.show()
interp.waitAtTop(done)
