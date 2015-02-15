String x = "testing123";
System.out.println(x.substring(n, n + m));
System.out.println(x.substring(n));
System.out.println(x.substring(0, x.length() - 1));
int index1 = x.indexOf('i');
System.out.println(x.substring(index1, index1 + m));
int index2 = x.indexOf("ing");
System.out.println(x.substring(index2, index2 + m));
//indexOf methods also have an optional "from index" argument which will
//make indexOf ignore characters before that index
