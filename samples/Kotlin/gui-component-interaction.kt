import java.awt.GridLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.KeyEvent
import java.awt.event.KeyListener

import java.lang.Long // jet.Long doesn't suit our needs here
import javax.swing.JButton
import javax.swing.JFrame
import javax.swing.JOptionPane
import javax.swing.JPanel
import javax.swing.JTextField

class Interact : JFrame() {
    val numberField = JTextField()
    val incButton = JButton("Increment")
    val randButton = JButton("Random")
    val buttonPanel = JPanel();

    {
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        numberField setText "0"

        numberField addKeyListener(object : KeyListener {
            public override fun keyTyped(e : KeyEvent) : Unit =
                if(!Character.isDigit(e.getKeyChar())) e.consume()
            public override fun keyReleased(e : KeyEvent?) {}
            public override fun keyPressed(e : KeyEvent) {}
        });

        incButton addActionListener(object : ActionListener {
            public override fun actionPerformed(e : ActionEvent) {
                val num = Long.parseLong(numberField.getText() ?: "")
                numberField setText (num + 1).toString()
            }
        });

        randButton addActionListener(object : ActionListener {
            fun proceedOrNot() = JOptionPane.showConfirmDialog(randButton, "Are you sure?")
            public override fun actionPerformed(e : ActionEvent) {
                if(proceedOrNot() == JOptionPane.YES_OPTION)
                    numberField setText Long.toString((Math.random() * Long.MAX_VALUE).toLong())
            }
        });

        setLayout(GridLayout(2, 1))
        buttonPanel setLayout GridLayout(1, 2)
        buttonPanel add incButton
        buttonPanel add randButton
        add(numberField)
        add(buttonPanel)
        pack()
    }
}

fun main(args : Array<String>) {
    Interact() setVisible(true)
}
