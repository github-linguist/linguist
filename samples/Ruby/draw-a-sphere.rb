Shoes.app :width => 500, :height => 500, :resizable => false do
  image 400, 470, :top => 30, :left => 50 do
    nostroke
    fill "#127"
    image :top => 230, :left => 0 do
      oval 70, 130, 260, 40
      blur 30
    end
    oval 10, 10, 380, 380
    image :top => 0, :left => 0 do
      fill "#46D"
      oval 30, 30, 338, 338
      blur 10
    end
    fill gradient(rgb(1.0, 1.0, 1.0, 0.7), rgb(1.0, 1.0, 1.0, 0.0))
    oval 80, 14, 240, 176
    image :top => 0, :left => 0 do
      fill "#79F"
      oval 134, 134, 130, 130
      blur 40
    end
    image :top => 150, :left => 40, :width => 320, :height => 260 do
      fill gradient(rgb(0.7, 0.9, 1.0, 0.0), rgb(0.7, 0.9, 1.0, 0.6))
      oval 60, 60, 200, 136
      blur 20
    end
  end
end
