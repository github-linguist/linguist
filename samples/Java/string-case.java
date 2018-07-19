String str = "alphaBETA";
System.out.println(str.toUpperCase());
System.out.println(str.toLowerCase());
//Also works with non-English characters with no modification
System.out.println("äàâáçñßæεбế".toUpperCase());
System.out.println("ÄÀÂÁÇÑSSÆΕБẾ".toLowerCase()); //does not transalate "SS" to "ß"
