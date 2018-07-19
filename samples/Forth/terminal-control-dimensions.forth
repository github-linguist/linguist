variable term-width
variable term-height

s" gforth" environment? [if]
  2drop form ( height width )
[else]  \ SwiftForth
  get-size ( width height ) swap
[then]
term-width ! term-height !
