USING: accessors alarms arrays calendar colors.constants kernel
locals math math.constants math.functions math.rectangles
math.vectors opengl sequences system ui ui.gadgets ui.render ;
IN: pendulum

CONSTANT: g 9.81
CONSTANT: l 20
CONSTANT: theta0 0.5

: current-time ( -- time ) nano-count -9 10^ * ;

: T0 ( -- T0 ) 2 pi l g / sqrt * * ;
: omega0 ( -- omega0 ) 2 pi * T0 / ;
: theta ( -- theta ) current-time omega0 * cos theta0 * ;

: relative-xy ( theta l -- xy )
    swap [ sin * ] [ cos * ] 2bi 2array ;
: theta-to-xy ( origin theta l -- xy ) relative-xy v+ ;

TUPLE: pendulum-gadget < gadget alarm ;

: O ( gadget -- origin ) rect-bounds [ drop ] [ first 2 / ] bi* 0 2array ;
: window-l ( gadget -- l ) rect-bounds [ drop ] [ second ] bi* ;
: gadget-xy ( gadget -- xy ) [ O ] [ drop theta ] [ window-l ] tri theta-to-xy ;

M: pendulum-gadget draw-gadget*
    COLOR: black gl-color
    [ O ] [ gadget-xy ] bi gl-line ;

M:: pendulum-gadget graft* ( gadget -- )
    [ gadget relayout-1 ]
    20 milliseconds every gadget (>>alarm) ;
M: pendulum-gadget ungraft* alarm>> cancel-alarm ;

: <pendulum-gadget> ( -- gadget )
    pendulum-gadget new
    { 500 500 } >>pref-dim ;
: pendulum-main ( -- )
    [ <pendulum-gadget> "pendulum" open-window ] with-ui ;
MAIN: pendulum-main
