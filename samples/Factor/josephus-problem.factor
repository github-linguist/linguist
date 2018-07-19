USING: kernel locals math math.ranges sequences ;
IN: josephus

:: josephus ( k n -- m )
    n [1,b] 0 [ [ k + ] dip mod ] reduce ;
