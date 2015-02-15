import java.util.Random;

public class FuzzyCircle {
	static final Random rnd = new Random();
	public static void main(String[] args){
		char[][] field = new char[31][31];
		for(int i = 0; i < field.length; i++){
			for(int j = 0; j < field[i].length; j++){
				field[i][j] = ' ';
			}
		}
		int pointsInDisc = 0;
		while(pointsInDisc < 100){
			int x = rnd.nextInt(31) - 15;
			int y = rnd.nextInt(31) - 15;
			double dist = Math.hypot(x, y);
			if(dist >= 10 && dist <= 15 && field[x + 15][y + 15] == ' '){
				field[x + 15][y + 15] = 'X';
				pointsInDisc++;
			}
		}
		for(char[] row:field){
			for(char space:row){
				System.out.print(space);
			}
			System.out.println();
		}
	}
}
