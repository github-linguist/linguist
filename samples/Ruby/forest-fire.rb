require 'enumerator'

def transition arr, tree_prob, fire_prob
  arr.enum_with_index.map do |cell, i|
    if i == 0 or i == arr.length - 1
      # boundary conditions: cells are always empty here
      :empty
    else
      case cell
      when :fire
        # burning cells become empty
        :empty
      when :empty
        # empty cells grow a tree with probability tree_prob
        rand < tree_prob ? :tree : :empty
      when :tree
        # check my neighbouring cells, are they on fire?
        if arr[i - 1] == :fire or arr[i + 1] == :fire
          :fire
        else
          # neighbours not on fire, but catch fire at random
          rand < fire_prob ? :fire : :tree
        end
      end
    end
  end
end

def pretty_print arr
  # colour the trees green, the fires red, and the empty spaces black
  print(arr.map { |cell|
    "\e[3" +
      case cell
      when :tree
        "2mT"
      when :fire
        "1mF"
      when :empty
        "0m "
      end + "\e[0m"
  }.join)
end

N = 20    # 20 trees
P = 0.5   # probability of growing a tree
F = 0.1   # probability of catching on fire

srand Time.now.to_i

# each cell has a 50/50 chance of being a tree
array = (1..N).map { rand < 0.5 ? :tree : :empty }
array[0] = array[-1] = :empty  # boundary conditions
pretty_print array
puts

begin
  array = transition(array, P, F)

  pretty_print array
end while gets.chomp.downcase != "q"
