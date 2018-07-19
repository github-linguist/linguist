dirs=: 0 1,1 0,0 _1,:_1 0
langton=:3 :0
  loc=. <.-:$cells=. (_2{.y,y)$dir=. 0
  while. *./(0<:loc), loc<$cells do.
    color=. (<loc) { cells
    cells=. (-.color) (<loc)} cells
    dir=. 4 | dir +  _1 ^ color
    loc=. loc + dir { dirs
  end.
  ' #' {~ cells
)
