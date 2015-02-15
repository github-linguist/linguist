include lib/memcell.4th
include 4pp/lib/foos.4pp

:: Point                               ( xn n a--)
   class
     field:  x                         \ x coordinate
     field:  y                         \ y coordinate
     method: print                     \ print routine
     method: setx                      \ set x coordinate
     method: sety                      \ set y coordinate
     method: getx                      \ get x coordinate
     method: gety                      \ get y coordinate
   end-class {
                                       \ bind the methods immediately
     :method { this -> x ! } ; defines setx
     :method { this -> y ! } ; defines sety
     :method { this -> x @ } ; defines getx
     :method { this -> y @ } ; defines gety
                                       \ because we'll use them immediately
     :method {                         \ e.g. in this print routine
       ." Point(" this => getx 0 .r ." ," this => gety 0 .r ." )" cr
     } ; defines print                 \ and this initialization
                                       \ object or argument count
     dup type@ this type@ =            \ if it is an object, a point
     if                                \ get the coordinates and set them
       dup => getx this => setx
           => gety this => sety
     else                              \ otherwise initialize it
       0 dup this => setx this => sety
       case                            \ and check the argument count
         1 of this => setx endof       \ one argument : x only
         2 of this => setx             \ two arguments: x and y
              this => sety endof
       endcase
     then

     private{ x y }                    \ make x and y private
   }
;

:: Circle                              ( xn n a --)
   over >r                             ( arg-count object-addr)
   extends Point                       \ save the argument count!!
     field:  r                         \ radius
     method: getr                      \ get radius
     method: setr                      \ set radius
   end-extends r> swap {               \ retrieve count
                                       \ bind the methods immediately
     :method { this -> r ! } ; defines setr
     :method { this -> r @ } ; defines getr
                                       \ because we'll use them immediately
     :method {                         \ e.g. in this print routine
       ." Circle(" this => getx 0 .r ." ,"
                   this => gety 0 .r ." ,"
                   this => getr 0 .r ." )" cr
     } ; defines print                 \ and this initialization
                                       \ object or argument count
     dup type@ this type@ =            \ if it is an object, a circle
     if                                \ get the coordinates and set them
       dup => getx this => setx
       dup => gety this => sety
           => getr this => setr
     else                              \ otherwise initialize it
       0 this => setr
       case                            \ and check the argument count
         3 of this => setr             \ three arguments: x, y and r
              this => sety             \ note the rest is already set
              this => setx endof       \ by "Point" and r was left on
       endcase                         \ the stack!
     then

     private{ r }
   }
;

0 new Point Point1
Point1 => print
45 23 2 new Point Point2
Point2 => print
Point2 new Point Point3
Point3 => print
78 1 new Point Point4
Point4 => print
10 45 23 3 new Circle Circle1
Circle1 => print
Point2 new Circle Circle2
Circle2 => print
Circle1 new Circle Circle3
Circle3 => print
