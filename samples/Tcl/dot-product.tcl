package require math::linearalgebra

set a {1 3 -5}
set b {4 -2 -1}
set dotp [::math::linearalgebra::dotproduct $a $b]
proc pp vec {return \[[join $vec ,]\]}
puts "[pp $a] \u2219 [pp $b] = $dotp"
