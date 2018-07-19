USING: arrays kernel locals math math.functions
 rosettacode.raster.storage sequences ;
IN: rosettacode.raster.line

! This gives a function
:: (quadratic-bezier) ( P0 P1 P2 -- bezier )
    [ :> x
        1 x - sq P0 n*v
        2 1 x - x * * P1 n*v
        x sq P2 n*v
        v+ v+ ] ; inline

! Same code from the cubic bezier task
: t-interval ( x -- interval )
    [ iota ] keep 1 - [ / ] curry map ;
: points-to-lines ( seq -- seq )
    dup rest [ 2array ] 2map ;
: draw-lines ( {R,G,B} points image -- )
    [ [ first2 ] dip draw-line ] curry with each ;
:: bezier-lines ( {R,G,B} P0 P1 P2 image -- )
    100 t-interval P0 P1 P2 (quadratic-bezier) map
    points-to-lines
    {R,G,B} swap image draw-lines ;
