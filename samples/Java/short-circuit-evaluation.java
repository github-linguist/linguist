public class ShortCirc {
    public static void main(String[] args){
        System.out.println("F and F = " + (a(false) && b(false)) + "\n");
        System.out.println("F or F = " + (a(false) || b(false)) + "\n");

        System.out.println("F and T = " + (a(false) && b(true)) + "\n");
        System.out.println("F or T = " + (a(false) || b(true)) + "\n");

        System.out.println("T and F = " + (a(true) && b(false)) + "\n");
        System.out.println("T or F = " + (a(true) || b(false)) + "\n");

        System.out.println("T and T = " + (a(true) && b(true)) + "\n");
        System.out.println("T or T = " + (a(true) || b(true)) + "\n");
    }

    public static boolean a(boolean a){
        System.out.println("a");
        return a;
    }

    public static boolean b(boolean b){
        System.out.println("b");
        return b;
    }
}
