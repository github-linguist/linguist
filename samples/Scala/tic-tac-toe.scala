package object tictactoe {
  val Human = 'X'
  val Computer = 'O'
  val BaseBoard = ('1' to '9').toList
  val WinnerLines = List((0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7), (2,5,8), (0,4,8), (2,4,6))
  val randomGen = new util.Random(System.currentTimeMillis)
}

package tictactoe {

class Board(aBoard : List[Char] = BaseBoard) {

  def availableMoves = aBoard.filter(c => c != Human && c != Computer)

  def availableMovesIdxs = for ((c,i) <- aBoard.zipWithIndex if c != Human && c != Computer) yield i

  def computerPlays = new Board(aBoard.updated(availableMovesIdxs(randomGen.nextInt(availableMovesIdxs.length)), Computer))

  def humanPlays(move : Char) = new Board(aBoard.updated(aBoard.indexOf(move), Human))

  def isDraw = aBoard.forall(c => c == Human || c == Computer)

  def isWinner(winner : Char) =
    WinnerLines.exists{case (i,j,k) => aBoard(i) == winner && aBoard(j) == winner && aBoard(k) == winner}

  def isOver = isWinner(Computer) || isWinner(Human) || isDraw

  def print {
    aBoard.grouped(3).foreach(row => println(row(0) + " " + row(1) + " " + row(2)))
  }

  def printOverMessage {
    if (isWinner(Human)) println("You win.")
    else if (isWinner(Computer)) println("Computer wins.")
    else if (isDraw) println("It's a draw.")
    else println("Not over yet, or something went wrong.")
  }

}


object TicTacToe extends Application {

   def play(board : Board, turn : Char) {

    // Reads a char from input until it is one of
    // the available moves in the current board
    def readValidMove() : Char = {
      print("Choose a move: ")
      val validMoves = board.availableMoves
      val move = readChar
      if (validMoves.contains(move)) {
        move
      } else {
        println("Invalid move. Choose another one in " + validMoves)
        readValidMove()
      }
    }


    board.print

    if (board.isOver) {
      board.printOverMessage
      return
    }

    if (turn == Human) { // Human plays
      val nextBoard = board.humanPlays(readValidMove)
      play(nextBoard, Computer)
    } else { // Computer plays
      println("Computer plays: ")
      val nextBoard = board.computerPlays
      play(nextBoard, Human)
    }
  }

  play(new Board(),Human)

}

}
