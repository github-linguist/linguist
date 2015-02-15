powers = function(m)
   {n = -1
    function()
       {n <<- n + 1
        n^m}}

noncubic.squares = local(
   {squares = powers(2)
    cubes = powers(3)
    cube = cubes()
    function()
       {square = squares()
        while (1)
           {if (square > cube)
               {cube <<- cubes()
                next}
            else if (square < cube)
               {return(square)}
            else
               {square = squares()}}}})

for (i in 1:20)
    noncubic.squares()
for (i in 1:10)
    message(noncubic.squares())
