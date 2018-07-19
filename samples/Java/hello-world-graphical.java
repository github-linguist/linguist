import javax.swing.*;
import java.awt.*;

public class OutputSwing {

    public static void main(String[] args) {

        SwingUtilities.invokeLater(new Runnable(){
            public void run() {
                JOptionPane.showMessageDialog (null, "Goodbye, World!"); // in alert box
                JFrame frame = new JFrame("Goodbye, World!");            // on title bar
                JTextArea text = new JTextArea("Goodbye, World!");       // in editable area
                JButton button = new JButton("Goodbye, World!");         // on button

                frame.setLayout(new FlowLayout());
                frame.add(button);
                frame.add(text);
                frame.pack();
                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                frame.setVisible(true);
            }
        });
    }
}
