require "rational"

def play
  digits = Array.new(4){rand(1..9)}
  loop do
    guess = get_guess(digits)
    result = evaluate(guess)
    if result == 24.0
      puts "yes!"
      break
    else
      puts "nope: #{guess} = #{result}"
      puts "try again"
    end
  end
end

def get_guess(digits)
  loop do
    print "\nEnter your guess using #{digits}: "
    guess = gets.chomp

    # ensure input is safe to eval
    invalid_chars = guess.scan(%r{[^\d\s()+*/-]})
    unless invalid_chars.empty?
      puts "invalid characters in input: #{invalid_chars}"
      next
    end

    guess_digits = guess.scan(/\d/).map {|ch| ch.to_i}
    if guess_digits.sort != digits.sort
      puts "you didn't use the right digits"
      next
    end

    if guess.match(/\d\d/)
      puts "no multi-digit numbers allowed"
      next
    end

    return guess
  end
end

# convert expression to use rational numbers, evaluate, then return as float
def evaluate(guess)
  as_rat = guess.gsub(/(\d)/, 'Rational(\1,1)')
  begin
    eval "(#{as_rat}).to_f"
  rescue SyntaxError
    "[syntax error]"
  end
end

play
