% namespace path tcl::mathfunc ;# to import functions like abs() etc.
% proc cube x {expr {$x**3}}
% proc croot x {expr {$x**(1/3.)}}
% proc compose {f g} {list apply {{f g x} {{*}$f [{*}$g $x]}} $f $g}

% compose abs cube          ;# returns a partial command, without argument
apply {{f g x} {{*}$f [{*}$g $x]}} abs cube

% {*}[compose abs cube] -3  ;# applies the partial command to argument -3
27

% set forward [compose [compose sin cos] cube] ;# omitting to print result
% set backward [compose croot [compose acos asin]]
% {*}$forward 0.5
0.8372297964617733
% {*}$backward [{*}$forward 0.5]
0.5000000000000017
