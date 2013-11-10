patches-own [
  living?         ;; indicates if the cell is living
  live-neighbors  ;; counts how many neighboring cells are alive
]

to setup-blank
  clear-all
  ask patches [ cell-death ]
  reset-ticks
end

to setup-random
  clear-all
  ask patches
    [ ifelse random-float 100.0 < initial-density
      [ cell-birth ]
      [ cell-death ] ]
  reset-ticks
end

to cell-birth
  set living? true
  set pcolor fgcolor
end

to cell-death
  set living? false
  set pcolor bgcolor
end

to go
  ask patches
    [ set live-neighbors count neighbors with [living?] ]
  ;; Starting a new "ask patches" here ensures that all the patches
  ;; finish executing the first ask before any of them start executing
  ;; the second ask.  This keeps all the patches in synch with each other,
  ;; so the births and deaths at each generation all happen in lockstep.
  ask patches
    [ ifelse live-neighbors = 3
      [ cell-birth ]
      [ if live-neighbors != 2
        [ cell-death ] ] ]
  tick
end

to draw-cells
  let erasing? [living?] of patch mouse-xcor mouse-ycor
  while [mouse-down?]
    [ ask patch mouse-xcor mouse-ycor
      [ ifelse erasing?
        [ cell-death ]
        [ cell-birth ] ]
      display ]
end

