class PigGame
  Player = Struct.new(:name, :safescore, :score) do
    def bust!() self.score = safescore end
    def stay!() self.safescore = score end
    def roll!(die_sides) rand(1..die_sides) end
    def wants_to_roll?
      print("#{name} (#{safescore}, #{score})", ": Roll? (Y) ")
      ['Y','y',''].include?(gets.chomp)
    end
  end

  def bust?(roll) roll==1 ? (puts("Busted!",'') || true) : false end

  def play(names, maxscore=100, die_sides=6)
    rotation = names.map {|name| Player.new(name,0,0) }.cycle

    while player = rotation.next
      loop do
        if player.wants_to_roll?
          puts "Rolled: #{roll=player.roll!(die_sides)}"
          if bust?( roll )
            player.bust! && break
          else
            player.score += roll
            return player if player.score >= maxscore
          end
        else
          player.stay! && puts("Staying with #{player.safescore}!", '')
          break
        end
      end
    end
  end
end

print PigGame.new.play( %w|Samuel Elizabeth| ).name, " wins!"
