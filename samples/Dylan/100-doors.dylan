define method doors()
  let doors = make(<array>, fill: #f, size: 100);
  for (x from 0 below 100)
    for (y from x below 100 by x + 1)
      doors[y] := ~doors[y]
    end
  end;
  for (x from 1 to 100)
    if (doors[x - 1])
      format-out("door %d open\n", x)
    end
  end
end
