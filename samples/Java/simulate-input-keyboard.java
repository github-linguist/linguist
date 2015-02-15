import java.awt.Robot
public static void type(String str){
   Robot robot = new Robot();
   for(char ch:str.toCharArray()){
      if(Character.isUpperCase(ch)){
         robot.keyPress(KeyEvent.VK_SHIFT);
         robot.keyPress((int)ch);
         robot.keyRelease((int)ch);
         robot.keyRelease(KeyEvent.VK_SHIFT);
      }else{
         char upCh = Character.toUpperCase(ch);
         robot.keyPress((int)upCh);
         robot.keyRelease((int)upCh);
      }
   }
}
