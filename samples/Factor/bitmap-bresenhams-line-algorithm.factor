USING: accessors arrays kernel locals math math.functions
math.ranges math.vectors rosettacode.raster.display
rosettacode.raster.storage sequences ui.gadgets ;
IN: rosettacode.raster.line

:: line-points ( pt1 pt2 -- points )
    pt1 first2 :> y0! :> x0!
    pt2 first2 :> y1! :> x1!
    y1 y0 - abs x1 x0 - abs > :> steep
    steep [
        y0 x0 y0! x0!
        y1 x1 y1! x1!
    ] when
    x0 x1 > [
        x0 x1 x0! x1!
        y0 y1 y0! y1!
    ] when
    x1 x0 - :> deltax
    y1 y0 - abs :> deltay
    0 :> current-error!
    deltay deltax / abs :> deltaerr
    0 :> ystep!
    y0 :> y!
    y0 y1 < [ 1 ystep! ] [ -1 ystep! ] if
    x0 x1 1 <range> [
        y steep [ swap ] when 2array
        current-error deltaerr + current-error!
        current-error 0.5 >= [
            ystep y + y!
            current-error 1 - current-error!
        ] when
    ] { } map-as ;

! Needs rosettacode.raster.storage for the set-pixel function and to create the image
: draw-line ( {R,G,B} pt1 pt2 image -- )
    [ line-points ] dip
    [ set-pixel ] curry with each ;
