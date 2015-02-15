String original = "Mary had a X lamb";
String little = "little";
String replaced = original.replace("X", little); //does not change the original String
System.out.println(replaced);
//Alternative:
System.out.printf("Mary had a %s lamb.", little);
//Alternative:
String formatted = String.format("Mary had a %s lamb.", little);
System.out.println(formatted);
