: symmetric-diff ( a b -- c )
    [ diff ] [ swap diff ] 2bi append ;

{ "John" "Bob" "Mary" "Serena" } { "Jim" "Mary" "John" "Bob" } symmetric-diff .
