drawBresenhamLine = (x0, y0, x1, y1) ->
  dx = Math.abs(x1 - x0)
  sx = if x0 < x1 then 1 else -1
  dy = Math.abs(y1 - y0)
  sy = if y0 < y1 then 1 else -1
  err = (if dx>dy then dx else -dy) / 2

  loop
    setPixel(x0, y0)
    break if x0 == x1 && y0 == y1
    e2 = err
    if e2 > -dx
      err -= dy
      x0 += sx
    if e2 < dy
      err += dx
      y0 += sy
  null
