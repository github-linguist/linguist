def <widget> := <swt:widgets.*>
def SWT := <swt:makeSWT>

def frame := <widget:makeShell>(currentDisplay)
  frame.setText("Rosetta Code")
  frame.setBounds(30, 30, 230, 60)
  frame.addDisposeListener(def _ { to widgetDisposed(event) {
    interp.continueAtTop()
  }})

def label := <widget:makeLabel>(frame, SWT.getLEFT())
  label.setText("Goodbye, World!")
  swtGrid`$frame: $label`

frame.open()

interp.blockAtTop()
