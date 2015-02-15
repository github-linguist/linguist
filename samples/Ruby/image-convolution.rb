class Pixmap
  # Apply a convolution kernel to a whole image
  def convolute(kernel)
    newimg = Pixmap.new(@width, @height)
    pb = ProgressBar.new(@width) if $DEBUG
    @width.times do |x|
      @height.times do |y|
        apply_kernel(x, y, kernel, newimg)
      end
      pb.update(x) if $DEBUG
    end
    pb.close if $DEBUG
    newimg
  end

  # Applies a convolution kernel to produce a single pixel in the destination
  def apply_kernel(x, y, kernel, newimg)
    x0 = x==0 ? 0 : x-1
    y0 = y==0 ? 0 : y-1
    x1 = x
    y1 = y
    x2 = x+1==@width  ? x : x+1
    y2 = y+1==@height ? y : y+1

    r = g = b = 0.0
    [x0, x1, x2].zip(kernel).each do |xx, kcol|
      [y0, y1, y2].zip(kcol).each do |yy, k|
        r += k * self[xx,yy].r
        g += k * self[xx,yy].g
        b += k * self[xx,yy].b
      end
    end
    newimg[x,y] = RGBColour.new(luma(r), luma(g), luma(b))
  end

  # Function for clamping values to those that we can use with colors
  def luma(value)
    if value < 0
      0
    elsif value > 255
      255
    else
      value
    end
  end
end


# Demonstration code using the teapot image from Tk's widget demo
teapot = Pixmap.open('teapot.ppm')
[ ['Emboss',  [[-2.0, -1.0, 0.0],  [-1.0, 1.0, 1.0],  [0.0, 1.0, 2.0]]],
  ['Sharpen', [[-1.0, -1.0, -1.0], [-1.0, 9.0, -1.0], [-1.0, -1.0, -1.0]]],
  ['Blur',    [[0.1111,0.1111,0.1111],[0.1111,0.1111,0.1111],[0.1111,0.1111,0.1111]]],
].each do |label, kernel|
  savefile = 'teapot_' + label.downcase + '.ppm'
  teapot.convolute(kernel).save(savefile)
end
