combSort=:3 :0
  gap=. #y
  whilst.1 < gap+swaps do.
    swaps=. 0
    i=. i.2,gap=. 1 >. <.gap%1.25
    while.{:$i=.i #"1~ ({: i) < #y do.
      swaps=.swaps+#{:k=.i #"1~b=. >/ i{y
      i=. i+gap
      y=.((|.k){y) k} y
    end.
  end.
  y
)
