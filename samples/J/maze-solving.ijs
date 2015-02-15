NB. source Dijkstra_equal_weights graph
NB.
NB.        +   +---+---+
NB.        | 0   1   2 |  (sample cell numbers)
NB.        +---+   +   +
NB.        | 3   4 | 5
NB.        +---+---+---+
NB.
NB. graph =: 1;0 2 4;1 5;4;1 3;2
NB. The graph is a vector of boxed vectors of neighbors.

Dijkstra_equal_weights =: 4 : 0
 dist =. previous =. #&_ n =. # graph =. y [ source =. x
 dist =. 0 source } dist
 Q =. 0
 while. #Q do.
   u =. {.Q
   Q =. }.Q
   if. _ = u{dist do. break. end.
   for_v. >u{graph do.
     if. -. v e. previous do.
       alt =. >: u { dist
       if. alt < v { dist do.
         dist =. alt v } dist
         previous =. u v } previous
         if. v e. Q do.
           echo 'belch'
         else.
           Q =. Q,v
         end.
       end.
     end.
   end.
 end.
 dist;previous
)

path =. 3 : 0
  p =. <:#y
  while. _ > {:p do.
    p =. p,y{~{:p
  end.
  |.}:p
)

solve=:3 :0
  NB. convert walls to graph
  shape =. }.@$@:>
  ew =. (,.&0 ,: 0&,.)@>@{.  NB. east west doors
  ns =. (, &0 ,: 0&, )@>@{:
  cell_offsets =. 1 _1 1 _1 * 2 # 1 , {:@shape
  cell_numbers =. i.@shape
  neighbors =. (cell_numbers +"_ _1 cell_offsets *"_1 (ew , ns))y
  graph =. (|:@(,/"_1) <@-."1 0 ,@i.@shape)neighbors NB. list of boxed neighbors
  NB. solve it
  path , > {: 0 Dijkstra_equal_weights graph
)

display=:3 :0 NB. Monadic display copied from maze generation task
  size=. >.&$&>/y
  text=. (}:1 3$~2*1+{:size)#"1":size$<' '
  'hdoor vdoor'=. 2 4&*&.>&.> (#&,{@;&i./@$)&.> y
  ' ' (a:-.~0 1;0 2; 0 3;(2 1-~$text);(1 4&+&.> hdoor),,vdoor+&.>"0/2 1;2 2;2 3)} text
:
  a=. display y
  size=. >.&$&>/y
  columns=. {: size
  cells =. <"1(1 2&p.@<.@(%&columns) ,.  2 4&p.@(columns&|))x
  '+' cells } a  NB. exercise, replace cells with a gerund to draw arrows on the path.
)

   4 (display~ solve)@maze 9
┌   ┬───┬───┬───┬───┬───┬───┬───┬───┐
│ +         │           │           │
├   ┼───┼   ┼   ┼   ┼───┼   ┼   ┼   ┤
│ +   + │       │           │   │   │
├───┼   ┼───┼───┼───┼───┼───┼   ┼───┤
│   │ +   +   + │ +   +   + │       │
├   ┼───┼───┼   ┼   ┼───┼   ┼───┼───┤
│             +   + │     +   +   +
└───┴───┴───┴───┴───┴───┴───┴───┴───┘
