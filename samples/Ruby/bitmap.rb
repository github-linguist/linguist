class RGBColour
  def initialize(red, green, blue)
    unless red.between?(0,255) and green.between?(0,255) and blue.between?(0,255)
      raise ArgumentError, "invalid RGB parameters: #{[red, green, blue].inspect}"
    end
    @red, @green, @blue = red, green, blue
  end
  attr_reader :red, :green, :blue
  alias_method :r, :red
  alias_method :g, :green
  alias_method :b, :blue

  RED   = RGBColour.new(255,0,0)
  GREEN = RGBColour.new(0,255,0)
  BLUE  = RGBColour.new(0,0,255)
  BLACK = RGBColour.new(0,0,0)
  WHITE = RGBColour.new(255,255,255)
end

class Pixmap
  def initialize(width, height)
    @width = width
    @height = height
    @data = fill(RGBColour::WHITE)
  end
  attr_reader :width, :height

  def fill(colour)
    @data = Array.new(@width) {Array.new(@height, colour)}
  end

  def validate_pixel(x,y)
    unless x.between?(0, @width-1) and y.between?(0, @height-1)
      raise ArgumentError, "requested pixel (#{x}, #{y}) is outside dimensions of this bitmap"
    end
  end

  def [](x,y)
    validate_pixel(x,y)
    @data[x][y]
  end
  alias_method :get_pixel, :[]

  def []=(x,y,colour)
    validate_pixel(x,y)
    @data[x][y] = colour
  end
  alias_method :set_pixel, :[]=
end
