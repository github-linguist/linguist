COLORS   = %i(red green purple) #use [:red, :green, :purple] in Ruby < 2.0
SYMBOLS  = %i(oval squiggle diamond)
NUMBERS  = %i(one two three)
SHADINGS = %i(solid open striped)
DECK = COLORS.product(SYMBOLS, NUMBERS, SHADINGS)

def get_all_sets(hand)
  hand.combination(3).select do |candidate|
    grouped_features = candidate.flatten.group_by{|f| f}
    grouped_features.values.none?{|v| v.size == 2}
  end
end

def get_puzzle_and_answer(hand_size, num_sets_goal)
  begin
    hand = DECK.sample(hand_size)
    sets = get_all_sets(hand)
  end until sets.size == num_sets_goal
  [hand, sets]
end

def print_cards(cards)
  puts cards.map{|card| "  %-8s" * 4 % card}
  puts
end

def set_puzzle(deal, goal=deal/2)
  puzzle, sets = get_puzzle_and_answer(deal, goal)
  puts "Dealt #{puzzle.size} cards:"
  print_cards(puzzle)
  puts "Containing #{sets.size} sets:"
  sets.each{|set| print_cards(set)}
end

set_puzzle(9)
set_puzzle(12)
