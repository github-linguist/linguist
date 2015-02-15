Shoes.app(:width => 470, :height => 380) do
  PI = Shoes::TWO_PI/2

  strokewidth 1

  def yin_yang(x, y, radius)
    fill black; stroke black
    arc x, y, radius, radius, -PI/2, PI/2

    fill white; stroke white
    arc x, y, radius, radius, PI/2, -PI/2
    oval x-radius/4, y-radius/2, radius/2-1

    fill black; stroke black
    oval x-radius/4, y, radius/2-1
    oval x-radius/12, y-radius/4-radius/12, radius/6-1

    fill white; stroke white
    oval x-radius/12, y+radius/4-radius/12, radius/6-1

    nofill
    stroke black
    oval x-radius/2, y-radius/2, radius
  end

  yin_yang 190, 190, 360
  yin_yang 410, 90, 90
end
