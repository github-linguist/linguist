public class CountingInFactors{
    public static void main(String[] args){
        for(int i = 1; i<= 10; i++){
            System.out.println(i + " = "+ countInFactors(i));
        }

        for(int i = 9991; i <= 10000; i++){
        	System.out.println(i + " = "+ countInFactors(i));
        }
    }

    private static String countInFactors(int n){
        if(n == 1) return "1";

        StringBuilder sb = new StringBuilder();

        n = checkFactor(2, n, sb);
        if(n == 1) return sb.toString();

        n = checkFactor(3, n, sb);
        if(n == 1) return sb.toString();

        for(int i = 5; i <= n; i+= 2){
            if(i % 3 == 0)continue;

            n = checkFactor(i, n, sb);
            if(n == 1)break;
        }

        return sb.toString();
    }

    private static int checkFactor(int mult, int n, StringBuilder sb){
        while(n % mult == 0 ){
            if(sb.length() > 0) sb.append(" x ");
            sb.append(mult);
            n /= mult;
        }
        return n;
    }
}
