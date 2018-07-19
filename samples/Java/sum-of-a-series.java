public class Sum{
    public static double f(double x){
       return 1/(x*x);
    }

    public static void main(String[] args){
       double start = 1;
       double end = 1000;
       double sum = 0;

       for(double x = start;x <= end;x++) sum += f(x);

       System.out.println("Sum of f(x) from " + start + " to " + end +" is " + sum);
    }
}
