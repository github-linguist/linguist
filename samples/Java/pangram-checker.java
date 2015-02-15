public class Pangram {
    public static boolean isPangram(String test){
        for (char a = 'A'; a <= 'Z'; a++)
            if ((test.indexOf(a) < 0) && (test.indexOf((char)(a + 32)) < 0))
                return false;
        return true;
    }

    public static void main(String[] args){
        System.out.println(isPangram("the quick brown fox jumps over the lazy dog"));//true
        System.out.println(isPangram("the quick brown fox jumped over the lazy dog"));//false, no s
        System.out.println(isPangram("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));//true
        System.out.println(isPangram("ABCDEFGHIJKLMNOPQSTUVWXYZ"));//false, no r
        System.out.println(isPangram("ABCDEFGHIJKL.NOPQRSTUVWXYZ"));//false, no m
        System.out.println(isPangram("ABC.D.E.FGHI*J/KL-M+NO*PQ R\nSTUVWXYZ"));//true
        System.out.println(isPangram(""));//false
    }
}
