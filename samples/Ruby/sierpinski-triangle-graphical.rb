Shoes.app(:height=>540,:width=>540, :title=>"Sierpinski Triangle") do
  def triangle(slot, tri, color)
    x, y, len = tri
    slot.append do
      fill color
      shape do
        move_to(x,y)
        dx = len * Math::cos(Math::PI/3)
        dy = len * Math::sin(Math::PI/3)
        line_to(x-dx, y+dy)
        line_to(x+dx, y+dy)
        line_to(x,y)
      end
    end
  end
  @s = stack(:width => 520, :height => 520) {}
  @s.move(10,10)

  length = 512
  @triangles = [[length/2,0,length]]
  triangle(@s, @triangles[0], rgb(0,0,0))

  @n = 1
  animate(1) do
    if @n <= 7
      @triangles = @triangles.inject([]) do |sum, (x, y, len)|
        dx = len/2 * Math::cos(Math::PI/3)
        dy = len/2 * Math::sin(Math::PI/3)
        triangle(@s, [x, y+2*dy, -len/2], rgb(255,255,255))
        sum += [[x, y, len/2], [x-dx, y+dy, len/2], [x+dx, y+dy, len/2]]
      end
    end
    @n += 1
  end

  keypress do |key|
    case key
    when :control_q, "\x11" then exit
    end
  end
end
