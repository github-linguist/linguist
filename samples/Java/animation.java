import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Timer;
import java.util.TimerTask;
import javax.swing.JFrame;
import javax.swing.JLabel;

public class Rotate extends JFrame {
  String text = "Hello World! ";
  JLabel label = new JLabel(text);
  boolean rotRight = true;
  int startIdx = 0;

  public Rotate() {
    label.addMouseListener(new MouseAdapter() {
      @Override
      public void mouseClicked(MouseEvent evt) {
        rotRight = !rotRight;
      }
    });
    add(label);
    pack();
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    setVisible(true);
  }

  public static void main(String[] args) {
    final Rotate rot = new Rotate();
    TimerTask task = new TimerTask() {
      public void run() {
        if (rot.rotRight) {
          rot.startIdx++;
          if (rot.startIdx >= rot.text.length()) {
            rot.startIdx -= rot.text.length();
          }
        } else {
          rot.startIdx--;
          if (rot.startIdx < 0) {
            rot.startIdx += rot.text.length();
          }
        }
        rot.label.setText(getRotatedText(rot.text, rot.startIdx));
      }
    };
    Timer timer = new Timer(false);
    timer.schedule(task, 0, 500);
  }

  public static String getRotatedText(String text, int startIdx) {
    String ret = "";
    int i = startIdx;
    do {
      ret += text.charAt(i) + "";
      i++;
      i = i % text.length();
    } while (i != startIdx);
    return ret;
  }
}
