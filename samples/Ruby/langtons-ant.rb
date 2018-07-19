class Ant

  class OutOfBoundsException < StandardError; end

  class Plane
    def initialize(x, y)
      @size_x, @size_y = x, y
      @cells = Array.new(y) {Array.new(x, :white)}
    end

    def white?(px, py)
      @cells[py][px] == :white
    end

    def toggle_colour(px, py)
      @cells[py][px] = (white?(px, py) ? :black : :white)
    end

    def check_bounds(px, py)
      unless (0 <= px and px < @size_x) and (0 <= py and py < @size_y)
        raise OutOfBoundsException, "(#@size_x, #@size_y)"
      end
    end

    def to_s
      @cells.collect {|row|
        row.collect {|cell| cell == :white ? "." : "#"}.join + "\n"
      }.join
    end
  end

  dir_move = [[:north, [0,-1]], [:east, [1,0]], [:south, [0,1]], [:west, [-1,0]]]
  Move = Hash[dir_move]
  directions = dir_move.map{|dir, move| dir}       # [:north, :east, :south, :west]
  Right = Hash[ directions.zip(directions.rotate).to_a ]
  Left  = Right.invert

  def initialize(size_x, size_y, pos_x=size_x/2, pos_y=size_y/2)
    @plane = Plane.new(size_x, size_y)
    @pos_x, @pos_y = pos_x, pos_y
    @direction = :south
    @plane.check_bounds(@pos_x, @pos_y)
  end

  def run
    moves = 0
    loop do
      begin
        moves += 1
        move
      rescue OutOfBoundsException
        break
      end
    end
    moves
  end

  def move
    @plane.toggle_colour(@pos_x, @pos_y)
    advance
    if @plane.white?(@pos_x, @pos_y)
      @direction = Right[@direction]
    else
      @direction = Left[@direction]
    end
  end

  def advance
    dx, dy = Move[@direction]
    @pos_x += dx
    @pos_y += dy
    @plane.check_bounds(@pos_x, @pos_y)
  end

  def position
    "(#@pos_x, #@pos_y)"
  end

  def to_s
    @plane.to_s
  end
end

#
# the simulation
#
ant = Ant.new(100, 100)
moves = ant.run
puts "out of bounds after #{moves} moves: #{ant.position}"
puts ant
