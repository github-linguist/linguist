import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Random;

public class RPS {
	public enum Item{
		ROCK, PAPER, SCISSORS, /*LIZARD, SPOCK*/;
		public List<Item> losesToList;
		public boolean losesTo(Item other) {
			return losesToList.contains(other);
		}
		static {
			SCISSORS.losesToList = Arrays.asList(ROCK/*, SPOCK*/);
			ROCK.losesToList = Arrays.asList(PAPER/*, SPOCK*/);
			PAPER.losesToList = Arrays.asList(SCISSORS/*, LIZARD*/);
			/*
			SPOCK.losesToList = Arrays.asList(PAPER, LIZARD);
			LIZARD.losesToList = Arrays.asList(SCISSORS, ROCK);
			*/
                }
	}
	//EnumMap uses a simple array under the hood
	public final Map<Item, Integer> counts = new EnumMap<Item, Integer>(Item.class){{
		for(Item item:Item.values())
			put(item, 1);
	}};

	private int totalThrows = Item.values().length;

	public static void main(String[] args){
		RPS rps = new RPS();
		rps.run();
	}

	public void run() {
		Scanner in = new Scanner(System.in);
		System.out.print("Make your choice: ");
		while(in.hasNextLine()){
			Item aiChoice = getAIChoice();
			String input = in.nextLine();
			Item choice;
			try{
				choice = Item.valueOf(input.toUpperCase());
			}catch (IllegalArgumentException ex){
				System.out.println("Invalid choice");
				continue;
			}
			counts.put(choice, counts.get(choice) + 1);
			totalThrows++;
			System.out.println("Computer chose: " + aiChoice);
			if(aiChoice == choice){
				System.out.println("Tie!");
			}else if(aiChoice.losesTo(choice)){
				System.out.println("You chose...wisely. You win!");
			}else{
				System.out.println("You chose...poorly. You lose!");
			}
			System.out.print("Make your choice: ");
		}
	}

	private static final Random rng = new Random();
	private Item getAIChoice() {
		int rand = rng.nextInt(totalThrows);
		for(Map.Entry<Item, Integer> entry:counts.entrySet()){
			Item item = entry.getKey();
			int count = entry.getValue();
			if(rand < count){
				List<Item> losesTo = item.losesToList;
				return losesTo.get(rng.nextInt(losesTo.size()));
			}
			rand -= count;
		}
		return null;
	}
}
