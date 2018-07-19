USING: arrays kernel locals math math.functions
 rosettacode.raster.storage sequences ;
IN: rosettacode.raster.line

! this gives a function
:: (cubic-bezier) ( P0 P1 P2 P3 -- bezier )
    [ :> x
        1 x - 3 ^ P0 n*v
        1 x - sq 3 * x * P1 n*v
        1 x - 3 * x sq * P2 n*v
        x 3 ^ P3 n*v
        v+ v+ v+ ] ; inline
! gives an interval of x from 0 to 1 to map the bezier function
: t-interval ( x -- interval )
    [ iota ] keep 1 - [ / ] curry map ;
! turns a list of points into the list of lines between them
: points-to-lines ( seq -- seq )
    dup rest [ 2array ] 2map ;
: draw-lines ( {R,G,B} points image -- )
    [ [ first2 ] dip draw-line ] curry with each ;
:: bezier-lines ( {R,G,B} P0 P1 P2 P3 image -- )
    ! 100 is an arbitrary value.. could be given as a parameter..
    100 t-interval P0 P1 P2 P3 (cubic-bezier) map
    points-to-lines
    {R,G,B} swap image draw-lines ;
