#!/usr/bin/env rune

var toggles := []
var gets := []

# Set up GUI (and data model)
def frame := <swing:makeJFrame>("100 doors")
frame.getContentPane().setLayout(<awt:makeGridLayout>(10, 10))
for i in 1..100 {
  def component := <import:javax.swing.makeJCheckBox>(E.toString(i))
  toggles with= fn { component.setSelected(!component.isSelected()) }
  gets with= fn { component.isSelected() }
  frame.getContentPane().add(component)
}

# Set up termination condition
def done
frame.addWindowListener(def _ {
  to windowClosing(event) {
    bind done := true
  }
  match _ {}
})

# Open and close doors
def loop(step, i) {
  toggles[i] <- ()
  def next := i + step
  timer.whenPast(timer.now() + 10, fn {
    if (next >= 100) {
      if (step >= 100) {
        # Done.
      } else {
        loop <- (step + 1, step)
      }
    } else {
      loop <- (step, i + step)
    }
  })
}
loop(1, 0)

frame.pack()
frame.show()
interp.waitAtTop(done)
