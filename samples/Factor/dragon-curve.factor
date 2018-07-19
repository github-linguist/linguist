USING: accessors colors colors.hsv fry kernel locals math
math.constants math.functions opengl.gl typed ui ui.gadgets
ui.gadgets.canvas ui.render ;

IN: dragon

CONSTANT: depth 12

TUPLE: turtle
    { angle fixnum }
    { color float }
    { x float }
    { y float } ;

TYPED: nxt-color ( turtle: turtle -- turtle )
    [ [ 360 2 depth ^ /f + ] keep
      1.0 1.0 1.0 <hsva> >rgba-components glColor4d
    ] change-color ; inline

TYPED: draw-fwd ( x1: float y1: float x2: float y2: float -- )
    GL_LINES glBegin glVertex2d glVertex2d glEnd ; inline

TYPED:: fwd ( turtle: turtle l: float -- )
    turtle x>>
    turtle y>>
    turtle angle>> pi * 180 / :> ( x y angle )
    l angle [ cos * x + ] [ sin * y + ] 2bi :> ( dx dy )
    turtle x y dx dy [ draw-fwd ] 2keep [ >>x ] [ >>y ] bi* drop ; inline

TYPED: trn ( turtle: turtle d: fixnum -- turtle )
    '[ _ + ] change-angle ; inline

TYPED:: dragon' ( turtle: turtle l: float s: fixnum d: fixnum -- )
    s zero? [
        turtle nxt-color l fwd ! don't like this drop
    ] [
        turtle d  45 * trn l 2 sqrt / s 1 -  1 dragon'
        turtle d -90 * trn l 2 sqrt / s 1 - -1 dragon'
        turtle d  45 * trn drop
    ] if ;

: dragon ( -- )
    0 0 150 180 turtle boa 400 depth 1 dragon' ;

TUPLE: dragon-canvas < canvas ;

M: dragon-canvas draw-gadget* [ drop dragon ] draw-canvas ;
M: dragon-canvas pref-dim* drop { 640 480 } ;

MAIN-WINDOW: dragon-window { { title "Dragon Curve" } }
    dragon-canvas new-canvas >>gadgets ;

MAIN: dragon-window
