int i ;
writefln(i.sizeof) ;        // print 4
int[13] ints1 ;             // static integer array of length 13
writefln(ints1.sizeof) ;    // print 52
int[] ints2 = new int[13] ; // dynamic integer array, variable length, currently 13
writefln(ints2.sizeof) ;    // print 8, all dynamic array has this size
writefln(ints2.length) ;    // print 13, length is the number of allocated element in aggregated type
