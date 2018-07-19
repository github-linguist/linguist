class RGBColour
  def values
    [@red, @green, @blue]
  end
end

class Pixmap
  def save(filename)
    File.open(filename, 'w') do |f|
      f.puts "P6", "#{@width} #{@height}", "255"
      f.binmode
      @height.times do |y|
        @width.times do |x|
          f.print @data[x][y].values.pack('C3')
        end
      end
    end
  end
  alias_method :write, :save
end
