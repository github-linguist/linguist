public class Trims{
   public static String ltrim(String s){
      int i = 0;
      while (i < s.length() && Character.isWhitespace(s.charAt(i))){
         i++;
      }
      return s.substring(i);
   }

   public static String rtrim(String s){
      int i = s.length() - 1;
      while (i > 0 && Character.isWhitespace(s.charAt(i))){
         i--;
      }
      return s.substring(0, i + 1);
   }

   public static void main(String[] args){
      String s = " \t \r \n String with spaces  \t  \r  \n  ";
      System.out.println(ltrim(s));
      System.out.println(rtrim(s));
      System.out.println(s.trim()); //trims both ends
   }
}
