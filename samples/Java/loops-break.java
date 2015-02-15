import java.util.Random;

Random rand = new Random();
while(true){
    int a = rand.nextInt(20);
    System.out.println(a);
    if(a == 10) break;
    int b = rand.nextInt(20);
    System.out.println(b);
}
