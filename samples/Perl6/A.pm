# used in t/spec/S11-modules/nested.t 

BEGIN { @*INC.push('t/spec/packages') };

module A::A {
    use A::B;
}

# vim: ft=perl6
