import java.util.Random;
public class SevenSidedDice
{
	private static final Random rnd = new Random();
	public static void main(String[] args)
	{
		SevenSidedDice now=new SevenSidedDice();
		System.out.println("Random number from 1 to 7: "+now.seven());
	}
	int seven()
	{
		int v=21;
		while(v>20)
			v=five()+five()*5-6;
		return 1+v%7;
	}
	int five()
	{
		return 1+rnd.nextInt(5);
	}
}
