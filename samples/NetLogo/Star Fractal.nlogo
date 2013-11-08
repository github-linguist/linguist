turtles-own [
  moves         ;; how many sides the turtles has drawn so far
  star-side     ;; The length of the side that the turtle is drawing
  generation    ;; How many generations of turtles have come before it.  So,
]               ;; if the turtle was hatched from a turtle that was hatched
                ;; by another turtle, it's generation is 2

to setup
  clear-all
  crt 1 [
    set size 8                    ;; so turtles are easy to see
    set ycor max-pycor * 0.9      ;; place near top of world
    set star-side ycor * 1.97538  ;; length of the sides of the star it will draw
    pen-down
    set heading 180 + 18
    recolor
  ]
  reset-ticks
end

to go
  if not any? turtles [ stop ]
  ask turtles [
    fd star-side
    rt 180 + 36
    if generation < fractal-level [      ;; If it's not drawing at the deepest level of the figure,
      hatch 1 [                          ;;  hatch a new turtle to draw at a lower level
        set generation generation + 1
        set star-side star-side / 2
        recolor
        set moves 0
        rt 18
      ]
    ]
    set moves moves + 1
    if moves > 5 [ die ]        ;; When the turtle has completed its star, it's done
  ]
  tick
end

to recolor  ;; turtle procedure
  set color scale-color red (fractal-level - generation) -1 (fractal-level + 1)
end
@#$#@#$#@
GRAPHICS-WINDOW
274
10
686
443
100
100
2.0
1
10
1
1
1
0
0
0
1
-100
100
-100
100
1
1
1
ticks
30.0

BUTTON
146
42
211
75
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
32
88
248
121
fractal-level
fractal-level
0
4
3
1
1
NIL
HORIZONTAL

BUTTON
69
42
133
75
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
105
128
190
173
turtles
count turtles
3
1
11

PLOT
2
179
272
394
Turtles vs. Time
Time
Num Turtles
0.0
25.0
0.0
180.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

@#$#@#$#@
## WHAT IS IT?

This model creates a fractal-like structure consisting of stars drawn at various scales, the number of which is specified by the user.  The structure is drawn by generations of turtles that draw stars at a given scale and hatch successive generations to draw new ones at smaller scales.

## HOW TO USE IT

The FRACTAL-LEVEL slider controls the number of levels the structure is drawn in.  For instance, a fractal level of 0 results in a single star, while a fractal level of 1 draws a star with smaller stars that begin at each of its 5 corners.  This pattern is repeated as the level increases.

Click the SETUP button to initialize the structure and GO to begin drawing it.

## THINGS TO NOTICE

The figure drawn embodies the most important fractal quality: self-similar structure at smaller scales.  This means that no matter how far you "zoom in" to it, you will still see the same shape (namely, a star).  The fractal level determines to how small a scale this property holds, so the greater the level the more you can "zoom in" and still see stars.

Notice that at a fractal level greater than 1, the model begins with one turtle, which hatches new turtles at each corner, and these turtles similarly hatch new ones at each of their corners as well. This behavior is made possible by a powerful mathematical technique called recursion.  A process is called recursive when its workings involve "performing itself".  In this case, the process performed by each turtle at each step is to draw a new side of its star and then create new turtles to perform the very same process.  A helpful property of recursive processes is that the instructions are often short, because each performer is executing the same instructions. Hence the brevity of Star Fractal's procedures.

The ideas behind fractal scaling (the property of self-similar structures at different scales) and recursion are essentially the same.  This is suggested by the fact that a recursive process is able to generate a fractal-like structure, as in this model.  One way to think about it is that recursion applies these ideas to processes (like NetLogo models), and fractal scaling applies them to physical or mathematical structures (like the figure drawn by Star Fractal).

## THINGS TO TRY

Edit GO so that it's not a forever button, and/or use the speed slider to slow the model down, and note what happens during each tick.  Notice that what begins as a single drawing multiplies into many drawings, each of which is at a smaller scale but proceeds exactly like the larger one that spawned it.  Thus, recursion begets self-sameness at different scales.

Try the model initially with small fractal levels and work your way up to higher ones.  Try to figure out how many more stars are incorporated into the figure each time the level increases by one.  What kind of general rule you can come up with? That is, given a generation of turtles g, how many turtles will be in the generation g + 1?  Start from g = 0.  (This is a recursive rule.)

## EXTENDING THE MODEL

In increasing order of difficulty:

Change the color scaling so that the smaller stars are brighter than the larger ones.

Change the model so that it draws shapes other than stars.  Will any shape work?

The structure that this model draws is only "fractal-like", because even when you "zoom in" and look at smaller scales, the smaller stars aren't actually part of the larger ones; they're just connected to them at a vertex. An important property of fractals is that the larger structures are composed of the smaller ones, something which clearly doesn't hold here.  For a tougher exercise, try changing the model so that the smaller stars (or whatever shapes are being drawn) are actually part of the larger ones, for instance by drawing them in the middle of their sides.  Such a figure will be a lot less "noisy" than this one, in terms of how difficult it is to discern individual stars, and it will have different properties when you zoom in.

## NETLOGO FEATURES

This model makes use of an important NetLogo command: `hatch`.  `hatch` allows one turtle to create a new one on the same patch and give it some initial instructions.  In this case, the parent turtle sets the length of the star its offspring will draw, its initial heading, and several other variables.  Notice that the new turtle uses the values of its parent turtle's variables in the body of the hatch procedure.

Another useful primitive this model uses is `scale-color`.  It is used to make the brightness of the color the turtles are drawing inversely proportional to the turtle's generation, so that larger stars are brighter than smaller ones (this was done to make the larger ones more visible).  This code:

    scale-color red (fractal-level - generation) -1 (fractal-level + 1)

works as follows: Subtract the value of `generation` from that of `fractal-level`.  The higher that resulting value is on the scale from -1 to `(fractal-level + 1)`, the darker the shade of red the turtle will draw with.

## CREDITS AND REFERENCES
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.5
@#$#@#$#@
setup
while [any? turtles] [ go ]
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
