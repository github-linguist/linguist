class Pixmap
  PIXMAP_FORMATS = ["P3", "P6"]   # implemented output formats
  PIXMAP_BINARY_FORMATS = ["P6"]  # implemented output formats which are binary

  def write_ppm(ios, format="P6")
    if not PIXMAP_FORMATS.include?(format)
      raise NotImplementedError, "pixmap format #{format} has not been implemented"
    end
    ios.puts format, "#{@width} #{@height}", "255"
    ios.binmode if PIXMAP_BINARY_FORMATS.include?(format)
    @height.times do |y|
      @width.times do |x|
        case format
        when "P3" then ios.print @data[x][y].values.join(" "),"\n"
        when "P6" then ios.print @data[x][y].values.pack('C3')
        end
      end
    end
  end

  def save(filename, opts={:format=>"P6"})
    File.open(filename, 'w') do |f|
      write_ppm(f, opts[:format])
    end
  end

  def print(opts={:format=>"P6"})
    write_ppm($stdout, opts[:format])
  end

  def save_as_jpeg(filename, quality=75)
    pipe = IO.popen("convert ppm:- -quality #{quality} jpg:#{filename}", 'w')
    write_ppm(pipe)
    pipe.close
  end
end

image = Pixmap.open('file.ppm')
image.save_as_jpeg('file.jpg')
