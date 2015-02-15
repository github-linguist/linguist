import(
  :javax:swing, :JOptionPane, :JFrame, :JTextArea, :JButton
)
import java:awt:FlowLayout

JOptionPane showMessageDialog(nil, "Goodbye, World!")
button = JButton new("Goodbye, World!")
text = JTextArea new("Goodbye, World!")
window = JFrame new("Goodbye, World!") do(
  layout = FlowLayout new
  add(button)
  add(text)
  pack
  setDefaultCloseOperation(JFrame field:EXIT_ON_CLOSE)
  visible = true
)
