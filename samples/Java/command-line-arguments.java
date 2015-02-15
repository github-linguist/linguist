public class Arguments {
  public static void main(String[] args) {
     System.out.println("There are " + args.length + " arguments given.");
     for(int i = 0; i < args.length; i++)
        System.out.println("The argument #" + (i+1) + " is " + args[i] + " and is at index " + i);
  }
}
