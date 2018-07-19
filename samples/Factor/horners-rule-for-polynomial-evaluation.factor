: horner ( coeff x -- res )
    [ <reversed> 0 ] dip '[ [ _ * ] dip + ] reduce ;
