class RGBColour
  def to_grayscale
    luminosity = Integer(0.2126*@red + 0.7152*@green + 0.0722*@blue)
    self.class.new(luminosity, luminosity, luminosity)
  end
end

class Pixmap
  def to_grayscale
    gray = self.class.new(@width, @height)
    @width.times do |x|
      @height.times do |y|
        gray[x,y] = self[x,y].to_grayscale
      end
    end
    gray
  end
end
