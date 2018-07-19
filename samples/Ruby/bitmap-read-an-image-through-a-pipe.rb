class Pixmap
  def self.read_ppm(ios)
    format = ios.gets.chomp
    width, height = ios.gets.chomp.split.map {|n| n.to_i }
    max_colour = ios.gets.chomp

    if (not PIXMAP_FORMATS.include?(format)) or
        width < 1 or height < 1 or
        max_colour != '255'
    then
      ios.close
      raise StandardError, "file '#{filename}' does not start with the expected header"
    end
    ios.binmode if PIXMAP_BINARY_FORMATS.include?(format)

    bitmap = self.new(width, height)
    height.times do |y|
      width.times do |x|
        # read 3 bytes
        red, green, blue = case format
          when 'P3' then ios.gets.chomp.split
          when 'P6' then ios.read(3).unpack('C3')
        end
        bitmap[x,y] = RGBColour.new(red, green, blue)
      end
    end
    ios.close
    bitmap
  end

  def self.open(filename)
    read_ppm(File.open(filename, 'r'))
  end

  def self.open_from_jpeg(filename)
    read_ppm(IO.popen("convert jpg:#{filename} ppm:-", 'r'))
  end
end

bitmap = Pixmap.open_from_jpeg('file.jpg')
