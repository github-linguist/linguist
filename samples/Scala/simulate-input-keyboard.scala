import java.awt.Robot
import java.awt.event.KeyEvent

object Keystrokes extends App {
  def Keystroke(str: String) {
    val robot = new Robot();
    for (ch <- str.toCharArray()) {
      if (Character.isUpperCase(ch)) {
        robot.keyPress(KeyEvent.VK_SHIFT);
        robot.keyPress(ch);
        robot.keyRelease(ch);
        robot.keyRelease(KeyEvent.VK_SHIFT);
      } else {
        val upCh = Character.toUpperCase(ch);
        robot.keyPress(upCh);
        robot.keyRelease(upCh);
      }
    }
  }
}
