module ContainsUnicode {
    sub uc-and-join(*@things, :$separator = ', ') is export {
        @things».uc.join($separator)
    }
}

# vim: ft=perl6
