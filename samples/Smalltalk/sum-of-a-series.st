( (1 to: 1000) fold: [:sum :aNumber |
  sum + (aNumber squared reciprocal) ] ) asFloat displayNl.
