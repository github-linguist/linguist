package keybord.macro.demo;

import javax.swing.JFrame;
import javax.swing.JLabel;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

class KeyboardMacroDemo {
    public static void main( String [] args ) {
        final JFrame frame = new JFrame();

        String directions = "<html><b>Ctrl-S</b> to show frame title<br>"
                                 +"<b>Ctrl-H</b> to hide it</html>";

        frame.add( new JLabel(directions));

        frame.addKeyListener( new KeyAdapter(){
            public void keyReleased( KeyEvent e ) {
                if( e.isControlDown() && e.getKeyCode() == KeyEvent.VK_S){
                    frame.setTitle("Hello there");
                }else if( e.isControlDown() && e.getKeyCode() == KeyEvent.VK_H){
                    frame.setTitle("");
                }
            }
        });
        frame.pack();
        frame.setVisible(true);
    }
}
