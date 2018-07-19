Shoes.app(:title => "Mouse Position", :width => 400, :height => 400) do
  @position = para "Position : ?, ?", :size => 12, :margin => 10

  motion do |x, y|
    @position.text = "Position : #{x}, #{y}"
  end
end
