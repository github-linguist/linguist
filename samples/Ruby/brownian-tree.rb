require 'rubygems'
require 'RMagick'

NUM_PARTICLES = 1000
SIZE          = 800

def draw_brownian_tree world
  # set the seed
  world[rand SIZE][rand SIZE] = 1

  NUM_PARTICLES.times do
    # set particle's position
    px = rand SIZE
    py = rand SIZE

    loop do
      # randomly choose a direction
      dx = rand(3) - 1
      dy = rand(3) - 1

      if dx + px < 0 or dx + px >= SIZE or dy + py < 0 or dy + py >= SIZE
        # plop the particle into some other random location
        px = rand SIZE
        py = rand SIZE
      elsif world[py + dy][px + dx] != 0
        # bumped into something
        world[py][px] = 1
        break
      else
        py += dy
        px += dx
      end
    end
  end
end

world = Array.new(SIZE) { Array.new(SIZE, 0) }
srand Time.now.to_i

draw_brownian_tree world

img = Magick::Image.new(SIZE, SIZE) do
  self.background_color = "black"
end

draw = Magick::Draw.new
draw.fill "white"

world.each_with_index do |row, y|
  row.each_with_index do |colour, x|
    draw.point x, y if colour != 0
  end
end

draw.draw img
img.write "brownian_tree.bmp"
