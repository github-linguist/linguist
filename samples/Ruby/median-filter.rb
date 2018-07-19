class Pixmap
  def median_filter(radius=3)
    radius += 1 if radius.even?
    filtered = self.class.new(@width, @height)
    pb = ProgressBar.new(@height) if $DEBUG
    @height.times do |y|
      @width.times do |x|
        window = []
        (x - radius).upto(x + radius).each do |win_x|
          (y - radius).upto(y + radius).each do |win_y|
            win_x = 0 if win_x < 0
            win_y = 0 if win_y < 0
            win_x = @width-1 if win_x >= @width
            win_y = @height-1 if win_y >= @height
            window << self[win_x, win_y]
          end
        end
        # median
        filtered[x, y] = window.sort[window.length / 2]
      end
      pb.update(y) if $DEBUG
    end
    pb.close if $DEBUG
    filtered
  end
end

class RGBColour
  # refactoring
  def luminosity
    Integer(0.2126*@red + 0.7152*@green + 0.0722*@blue)
  end
  def to_grayscale
    l = luminosity
    self.class.new(l, l, l)
  end

  # defines how to compare (and hence, sort)
  def <=>(other)
    self.luminosity <=> other.luminosity
  end
end

class ProgressBar
  def initialize(max)
    $stdout.sync = true
    @progress_max = max
    @progress_pos = 0
    @progress_view = 68
    $stdout.print "[#{'-'*@progress_view}]\r["
  end

  def update(n)
    new_pos = n * @progress_view/@progress_max
    if new_pos > @progress_pos
      @progress_pos = new_pos
      $stdout.print '='
    end
  end

  def close
    $stdout.puts '=]'
  end
end

bitmap = Pixmap.open('file')
filtered = bitmap.median_filter
