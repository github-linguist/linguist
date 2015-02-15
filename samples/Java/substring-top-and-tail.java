public class RM_chars {
  public static void main( String[] args ){
    System.out.println( "knight".substring( 1 ) );
    System.out.println( "socks".substring( 0, 4 ) );
    System.out.println( "brooms".substring( 1, 5 ) );
      // first, do this by selecting a specific substring
      // to exclude the first and last characters

    System.out.println( "knight".replaceAll( "^.", "" ) );
    System.out.println( "socks".replaceAll( ".$", "" ) );
    System.out.println( "brooms".replaceAll( "^.|.$", "" ) );
      // then do this using a regular expressions
  }
}
