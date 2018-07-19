class Main {

	def input = new Scanner(System.in)
	
	static main(args) {
		Main main = new Main();
		main.play();
	}

	public void play() {

		TicTackToe game = new TicTackToe();
		game.init()
		def gameOver = false		
		def player = game.player1
		
		println "Players take turns marking a square. Only squares \n"+
				"not already marked can be picked. Once a player has \n"+
    			"marked three squares in a row, column or diagonal, they win! If all squares \n"+
    			"are marked and no three squares are the same, a tied game is declared.\n"+
				"Player 1 is O and Player 2 is X \n"+
    			"Have Fun! \n\n"
		println "${game.drawBoard()}"

		while (!gameOver && game.plays < 9) {
			
			player = game.currentPlayer == 1 ? game.player1 :game.player2
			def validPick = false;
			while (!validPick) {
				
				def square
				println "Next $player, enter move by selecting square's number :"	
				try {
					square = input.nextLine();
				} catch (Exception ex) { }					
				 			
				if (square.length() == 1 && Character.isDigit(square.toCharArray()[0])) {	validPick = game.placeMarker(square)	}
				if (!validPick) {	println "Select another Square"	}
				
			}
			
			(game.checkWinner(player))?	gameOver = true	: game.switchPlayers()			
			println(game.drawBoard());	
				
		}		
		println "Game Over, " + ((gameOver == true)? "$player Wins" : "Draw")
	}

}

public class TicTackToe {
	
    def board = new Object[3][3]
	def final player1 = "player 1"
	def final player2 = "player 2"
	def final marker1 = 'X'
	def final marker2 = 'O'
	
    int currentPlayer
	int plays

	public TicTacToe(){
		
	}


    def init() {
        int counter = 0;
       (0..2).each { row ->
           (0..2).each { col ->
                board[row][col] = (++counter).toString();
            }
        }
	   plays = 0
	   currentPlayer =1
    }

    def switchPlayers() {
        currentPlayer = (currentPlayer == 1) ? 2:1
        plays++
    }

    def placeMarker(play) {
		def result = false
        (0..2).each { row ->
            (0..2).each { col ->
                if (board[row][col].toString()==play.toString()){
                    board[row][col] = (currentPlayer == 1) ? marker2 : marker1;
                    result =  true;
                }
            }
        }
        return result;
    }

    def checkWinner(player) {
    	char current = (player == player1)? marker2:  marker1
        //Checking
        return checkRows(current) || checkColumns(current) ||checkDiagonals(current);
    }

    def checkRows(char current){
		(0..2).any{ line ->
			  board[line].every { it == current}
		}
    }


    def checkColumns(char current){				
		(0..2).any{i ->
			(0..2).every{j ->
				 board[j][i]==current }		
		}
    }

    def checkDiagonals(char current){
		def rightDiag = [board[0][0],board[1][1],board[2][2]]
		def leftDiag =  [board[0][2],board[1][1],board[2][0]]
		return rightDiag.every{it == current} || leftDiag.every{it == current}
    }


    def drawBoard() {
        StringBuilder builder = new StringBuilder("Game board: \n");
        (0..2).each { row->
            (0..2).each {col ->
                builder.append("[" + board[row][col] + "]");
            }
            builder.append("\n");
        }
        builder.append("\n");
        return builder.toString();
    }
}
