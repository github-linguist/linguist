import java.util.Arrays;

public class Shuffler {
	
	private int seed;
	
	private String[] deck = {
			"AC", "AD", "AH", "AS",
			"2C", "2D", "2H", "AS",
			"3C", "3D", "3H", "3S",
			"4C", "4D", "4H", "4S",
			"5C", "5D", "5H", "5S",
			"6C", "6D", "6H", "6S",
			"7C", "7D", "7H", "7S",
			"8C", "8D", "8H", "8S",
			"9C", "9D", "9H", "9S",
			"TC", "TD", "TH", "TS",
			"JC", "JD", "JH", "JS",
			"QC", "QD", "QH", "QS",
			"KC", "KD", "KH", "KS",
	};
	
	private int random() {
		seed = (214013 * seed + 2531011) & Integer.MAX_VALUE;
		return seed >> 16;
	}
	
	//shuffled cards go to the end
	private String[] getShuffledDeck() {
		String[] deck = Arrays.copyOf(this.deck, this.deck.length);
		for(int i = deck.length - 1; i > 0; i--) {
			int r = random() % (i + 1);
			String card = deck[r];
			deck[r] = deck[i];
			deck[i] = card;
		}
		return deck;
	}
	
	//deal from end first
	public void dealGame(int seed) {
		this.seed = seed;
		String[] shuffledDeck = getShuffledDeck();
		for(int count = 1, i = shuffledDeck.length - 1; i >= 0; count++, i--) {
			System.out.print(shuffledDeck[i]);
			if(count % 8 == 0) {
				System.out.println();
			} else {
				System.out.print(" ");
			}
		}
		System.out.println();
	}
	
	public static void main(String[] args) {
		Shuffler s = new Shuffler();
		s.dealGame(1);
		System.out.println();
		s.dealGame(617);
	}
	
}
