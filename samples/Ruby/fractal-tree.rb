Shoes.app(:title => "Fractal Tree", :width => 600, :height => 600) do
  background "#fff"
  stroke "#000"
  @deg_to_rad = Math::PI / 180.0

  def drawTree(x1, y1, angle, depth)
    if depth != 0
      x2 = x1 + (Math.cos(angle * @deg_to_rad) * depth * 10.0).to_i
      y2 = y1 + (Math.sin(angle * @deg_to_rad) * depth * 10.0).to_i

      line x1, y1, x2, y2

      drawTree(x2, y2, angle - 20, depth - 1)
      drawTree(x2, y2, angle + 20, depth - 1)
    end
  end

  drawTree(300,550,-90,9)
end
