public class Guessing {
    public static void main(String[] args) throws NumberFormatException{
        int n = (int)(Math.random() * 10 + 1);
        System.out.print("Guess the number between 1 and 10: ");
        while(Integer.parseInt(System.console().readLine()) != n){
            System.out.print("Wrong! Guess again: ");
        }
        System.out.println("Well guessed!");
    }
}
