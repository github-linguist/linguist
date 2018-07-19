public static boolean inCarpet(long x, long y) {
    while (x!=0 && y!=0) {
        if (x % 3 == 1 && y % 3 == 1)
            return false;
        x /= 3;
        y /= 3;
    }
    return true;
}

public static void carpet(final int n) {
    final double power = Math.pow(3,n);
    for(long i = 0; i < power; i++) {
        for(long j = 0; j < power; j++) {
            System.out.print(inCarpet(i, j) ? "*" : " ");
        }
        System.out.println();
    }
}
