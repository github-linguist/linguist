public static void main(String[] args){
    System.out.println(soundex("Soundex"));
    System.out.println(soundex("Example"));
    System.out.println(soundex("Sownteks"));
    System.out.println(soundex("Ekzampul"));
  }

private static String getCode(char c){
  switch(c){
    case 'B': case 'F': case 'P': case 'V':
      return "1";
    case 'C': case 'G': case 'J': case 'K':
    case 'Q': case 'S': case 'X': case 'Z':
      return "2";
    case 'D': case 'T':
      return "3";
    case 'L':
      return "4";
    case 'M': case 'N':
      return "5";
    case 'R':
      return "6";
    default:
      return "";
  }
}

public static String soundex(String s){
  String code, previous, soundex;
  code = s.toUpperCase().charAt(0) + "";
  previous = "7";
  for(int i = 1;i < s.length();i++){
    String current = getCode(s.toUpperCase().charAt(i));
    if(current.length() > 0 && !current.equals(previous)){
      code = code + current;
    }
    previous = current;
  }
  soundex = (code + "0000").substring(0, 4);
  return soundex;
}
