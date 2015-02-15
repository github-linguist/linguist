class Pixmap
  def histogram
    histogram = Hash.new(0)
    @height.times do |y|
      @width.times do |x|
        histogram[self[x,y].luminosity] += 1
      end
    end
    histogram
  end

  def to_blackandwhite
    hist = histogram

    # find the median luminosity
    median = nil
    sum = 0
    hist.keys.sort.each do |lum|
      sum += hist[lum]
      if sum > @height * @width / 2
        median = lum
        break
      end
    end

    # create the black and white image
    bw = self.class.new(@width, @height)
    @height.times do |y|
      @width.times do |x|
        bw[x,y] = self[x,y].luminosity < median ? RGBColour::BLACK : RGBColour::WHITE
      end
    end
    bw
  end

  def save_as_blackandwhite(filename)
    to_blackandwhite.save(filename)
  end
end

Pixmap.open('file.ppm').save_as_blackandwhite('file_bw.ppm')
