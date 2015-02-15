: endian
  cr 1 cells . ." address units per cell"
  s" ADDRESS-UNIT-BITS" environment? if cr . ." bits per address unit" then
  cr 1 here ! here c@ if ." little" else ." big" then ."  endian" ;
