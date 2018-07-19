class RockPaperScissorsGame
  def initialize()
    @pieces = %w[rock paper scissors]
    @beats = {
      'rock' => 'paper',
      'paper' => 'scissors',
      'scissors' => 'rock',
    }
    @plays = {
      'rock' => 1,
      'paper' => 1,
      'scissors' => 1,
    }
    @score = [0, 0]

    play
  end

  def humanPlay()
    answer = nil
    loop do
      print "\nYour choice: #@pieces? "
      answer = STDIN.gets.strip.downcase
      next if answer.empty?
      if idx = @pieces.find_index {|piece| piece.match(/^#{answer}/)}
        answer = @pieces[idx]
        break
      else
        puts "invalid answer, try again"
      end
    end
    answer
  end

  def computerPlay()
    total = @plays.values.reduce(:+)
    r = rand(total) + 1
    sum = 0
    humans_choice = nil
    @pieces.each do |piece|
      sum += @plays[piece]
      if r <= sum
        humans_choice = piece
        break
      end
    end
    @beats[humans_choice]
  end

  def play
    loop do
      h = humanPlay
      c = computerPlay
      print "H: #{h}, C: #{c} => "

      # only update the human player's history after the computer has chosen
      @plays[h] += 1

      if h == c
        puts "draw"
      elsif h == @beats[c]
        puts "Human wins"
        @score[0] += 1
      else
        puts "Computer wins"
        @score[1] += 1
      end
      puts "score: human=#{@score[0]}, computer=#{@score[1]}"
    end
  end
end

game = RockPaperScissorsGame.new
