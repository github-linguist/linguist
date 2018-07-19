module TicTacToe
  ROWS = [[1,2,3],[4,5,6],[7,8,9],[1,4,7],[2,5,8],[3,6,9],[1,5,9],[3,5,7]]

  class Game
    def initialize(player1Class, player2Class)
      @board = Array.new(10)
      @free_positions = (1..9).to_a
      @players = [player1Class.new(self), player2Class.new(self)]
      @turn = rand(2)
      puts "#{@players[@turn]} goes first."
      @players[@turn].marker = "X"
      @players[nextTurn].marker = "O"
    end
    attr_reader :free_positions, :board, :turn

    def play
      loop do
        player = @players[@turn]
        idx = player.select
        puts "#{player} selects #{player.marker} position #{idx}"
        @board[idx] = player.marker
        @free_positions.delete(idx)

        # check for a winner
        ROWS.each do |row|
          if row.all? {|idx| @board[idx] == player.marker}
            puts "#{player} wins!"
            print_board
            return
          end
        end

        # no winner, is board full?
        if @free_positions.empty?
          puts "It's a draw."
          print_board
          return
        end

        nextTurn!
      end
    end

    def nextTurn
      1 - @turn
    end

    def nextTurn!
      @turn = nextTurn
    end

    def opponent
      @players[nextTurn]
    end

    def print_board
      display =lambda{|row| row.map {|i| @board[i] ? @board[i] : i}.join("|")}
      puts display[[1,2,3]], "-+-+-", display[[4,5,6]], "-+-+-", display[[7,8,9]]
    end
  end

  class Player
    def initialize(game)
      @game = game
      @marker = nil
    end
    attr_accessor :marker
  end

  class HumanPlayer < Player
    def select
      @game.print_board
      loop do
        print "Select your #{marker} position: "
        selection = gets.to_i
        return selection if @game.free_positions.include?(selection)
        puts "Position #{selection} is not available. Try again."
      end
    end

    def to_s
      "Human"
    end
  end

  class ComputerPlayer < Player
    def group_row(row)
      markers = row.group_by {|idx| @game.board[idx]}
      markers.default = []
      markers
    end

    def select
      opponent_marker = @game.opponent.marker

      # look for winning rows
      for row in ROWS
        markers = group_row(row)
        next if markers[nil].length != 1
        if markers[self.marker].length == 2
          return markers[nil].first
        elsif markers[opponent_marker].length == 2
          idx = markers[nil].first
        end
      end

      # look for opponent's winning rows to block
      return idx if idx

      # need some logic here to get the computer to pick a smarter position

      # simply pick a position in order of preference
      ([5] + [1,3,7,9].shuffle + [2,4,6,8].shuffle).find do |pos|
        @game.free_positions.include?(pos)
      end
    end

    def to_s
      "Computer#{@game.turn}"
    end
  end
end

include TicTacToe

Game.new(ComputerPlayer, ComputerPlayer).play
puts
Game.new(HumanPlayer,ComputerPlayer).play
