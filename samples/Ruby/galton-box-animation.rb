$rows_of_pins = 12
$width = $rows_of_pins * 10 + ($rows_of_pins+1)*14

Shoes.app(
    :width => $width + 14,
    :title => "Galton Box"
) do
  @bins = Array.new($rows_of_pins+1, 0)

  @x_coords = Array.new($rows_of_pins) {Array.new}
  @y_coords = Array.new($rows_of_pins)
  stack(:width => $width) do
    stroke gray
    fill gray
    1.upto($rows_of_pins) do |row|
      y = 14 + 24*row
      @y_coords[row-1] = y
      row.times do |i|
        x = $width / 2 + (i - 0.5*row)*24 + 14
        @x_coords[row-1] << x
        oval x+2, y, 6
      end
    end
  end
  @y_coords << @y_coords[-1] + 24
  @x_coords << @x_coords[-1].map {|x| x-12} + [@x_coords[-1][-1]+12]

  @balls = stack(:width => $width) do
    stroke red
    fill red
  end.move(0,0)

  @histogram = stack(:width => $width) do
    nostroke
    fill black
  end.move(0, @y_coords[-1] + 10)

  @paused = false
  keypress do |key|
    case key
    when "\x11", :control_q
      exit
    when "\x10", :control_p
      @paused = !@paused
    end
  end

  @ball_row = 0
  @ball_col = 0
  animate(2*$rows_of_pins) do
    if not @paused
      y = @y_coords[@ball_row] - 12
      x = @x_coords[@ball_row][@ball_col]
      @balls.clear {oval x, y, 10}
      @ball_row += 1
      if @ball_row <= $rows_of_pins
        @ball_col += 1 if rand >= 0.5
      else
        @bins[@ball_col] += 1
        @ball_row = @ball_col = 0
        update_histogram
      end
    end
  end

  def update_histogram
    y = @y_coords[-1] + 10
    @histogram.clear do
      @bins.each_with_index do |num, i|
        if num > 0
          x = @x_coords[-1][i]
          rect x-6, 0, 24, num
        end
      end
    end
  end
end
