puts <<EOS
    Minesweeper game.

    There is an n by m grid that has a random number of between 20% to 60%
    of randomly hidden mines that need to be found.

    Positions in the grid are modified by entering their coordinates
    where the first coordinate is horizontal in the grid and the second
    vertical. The top left of the grid is position 1,1; the bottom right is
    at n,m.

    * The total number of mines to be found is shown at the beginning of the
    game.
    * Each mine occupies a single grid point, and its position is initially
    unknown to the player
    * The grid is shown as a rectangle of characters between moves.
    * You are initially shown all grids as obscured, by a single dot '.'
    * You may mark what you think is the position of a mine which will show
    as a '?'
    * You can mark what you think is free space by entering its coordinates.
    :*  If the point is free space then it is cleared, as are any adjacent
    points that are also free space- this is repeated recursively for
    subsequent adjacent free points unless that point is marked as a mine
    or is a mine.
    ::*   Points marked as a mine show as a '?'.
    ::*   Other free points show as an integer count of the number of adjacent
    true mines in its immediate neighbourhood, or as a single space ' ' if the
    free point is not adjacent to any true mines.
    * Of course you loose if you try to clear space that starts on a mine.
    * You win when you have correctly identified all mines.


    When prompted you may:
        Toggle where you think a mine is at position x, y:
          m <x> <y>
        Clear the grid starting at position x, y (and print the result):
          c <x> <y>
        Print the grid so far:
          p
        Quit
          q
    Resigning will first show the grid with an 'N' for unfound true mines, a
    'Y' for found true mines and a '?' for where you marked clear space as a
    mine
EOS

WIDTH, HEIGHT = 6, 4
PCT = 0.15
NUM_MINES = (WIDTH * HEIGHT * PCT).round

def create_mines sx, sy
  arr = Array.new(WIDTH) { Array.new(HEIGHT, false) }
  NUM_MINES.times do
    x, y = rand(WIDTH), rand(HEIGHT)
    # place it if it isn't at (sx, sy) and we haven't already placed a mine
    redo if arr[x][y] or (x == sx and y == sy)
    arr[x][y] = true
  end
  arr
end

def num_marks
  $screen.inject(0) { |sum, row| sum + row.count("?") }
end

def show_grid revealed = false
  if revealed
    puts $mines.transpose.map { |row| row.map { |cell| cell ? "*" : " " }.join(" ") }
  else
    puts "Grid has #{NUM_MINES} mines, #{num_marks} marked."
    puts $screen.transpose.map{ |row| row.join(" ") }
  end
end

SURROUND = [-1,0,1].product([-1,0,1]) - [[0,0]]     # surround 8
def surrounding x, y
  # apply the passed block to each spot around (x, y)
  SURROUND.each do |dx, dy|
    # don't check if we're out of bounds, or at (0,0)
    yield(x+dx, y+dy) if (0...WIDTH).cover?(x+dx) and (0...HEIGHT).cover?(y+dy)
  end
end

def clear_space x, y
  return unless $screen[x][y] == "."
  # check nearby spaces
  count = 0
  surrounding(x, y) { |px, py| count += 1 if $mines[px][py] }
  if count == 0
    $screen[x][y] = " "
    surrounding(x, y) { |px, py| clear_space px, py }
  else
    $screen[x][y] = count.to_s
  end
end

def victory?
  return false if $mines.nil?  # first one, don't need to check
  return false if num_marks != NUM_MINES
  mines_left = NUM_MINES
  WIDTH.times do |x|
    HEIGHT.times do |y|
      mines_left -= 1 if $mines[x][y] and $screen[x][y] == "?"
    end
  end

  mines_left == 0
end

def check_input x, y
  x, y = x.to_i - 1, y.to_i - 1
  [x, y] if (0...WIDTH).cover?(x) and (0...HEIGHT).cover?(y)
end

$mines = nil
$screen = Array.new(WIDTH) { Array.new(HEIGHT, ".") }

puts "Welcome to Minesweeper!"
show_grid

loop do
  print "> "
  action = gets.chomp.downcase

  case action
  when "quit", "exit", "x", "q"
    puts "Bye!"
    break
  when /^m (\d+) (\d+)$/
    # mark this cell
    x, y = check_input $1, $2
    next unless x
    if $screen[x][y] == "."
      # mark it
      $screen[x][y] = "?"
      if victory?
        show_grid
        puts "You win!"
        break
      end
    elsif $screen[x][y] == "?"
      # unmark it
      $screen[x][y] = "."
    end
    show_grid
  when /^c (\d+) (\d+)$/
    x, y = check_input $1, $2
    next unless x
    $mines ||= create_mines(x, y)
    if $mines[x][y]
      puts "You hit a mine!"
      show_grid true
      break
    else
      clear_space x, y
      show_grid
      if victory?
        puts "You win!"
        break
      end
    end
  when "p"
    show_grid
  end
end
