class Pixmap
  # 'open' is a class method
  def self.open(filename)
    bitmap = nil
    File.open(filename, 'r') do |f|
      header = [f.gets.chomp, f.gets.chomp, f.gets.chomp]
      width, height = header[1].split.map {|n| n.to_i }
      if header[0] != 'P6' or header[2] != '255' or width < 1 or height < 1
        raise StandardError, "file '#{filename}' does not start with the expected header"
      end
      f.binmode
      bitmap = self.new(width, height)
      height.times do |y|
        width.times do |x|
          # read 3 bytes
          red, green, blue = f.read(3).unpack('C3')
          bitmap[x,y] = RGBColour.new(red, green, blue)
        end
      end
    end
    bitmap
  end
end

# create an image: a green cross on a blue background
colour_bitmap = Pixmap.new(20, 30)
colour_bitmap.fill(RGBColour::BLUE)
colour_bitmap.height.times {|y| [9,10,11].each {|x| colour_bitmap[x,y]=RGBColour::GREEN}}
colour_bitmap.width.times  {|x| [14,15,16].each {|y| colour_bitmap[x,y]=RGBColour::GREEN}}
colour_bitmap.save('testcross.ppm')

# then, convert to grayscale
Pixmap.open('testcross.ppm').to_grayscale!.save('testgray.ppm')
