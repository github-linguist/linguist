: logical-operators ( a b -- )
    {
        [ "xor is: " write xor . ]
        [ "and is: " write and . ]
        [ "or is:  " write or . ]
        [ "not is: " write drop not . ]
    } 2cleave ;
