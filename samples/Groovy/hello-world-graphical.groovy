import groovy.swing.SwingBuilder
import javax.swing.JFrame

new SwingBuilder().edt {
  optionPane().showMessageDialog(null, "Goodbye, World!")
  frame(title:'Goodbye, World!', defaultCloseOperation:JFrame.EXIT_ON_CLOSE, pack:true, show: true) {
    flowLayout()
    button(text:'Goodbye, World!')
    textArea(text:'Goodbye, World!')
  }
}
