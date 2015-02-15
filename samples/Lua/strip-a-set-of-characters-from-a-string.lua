function stripchars( str, chr )
    local s = ""
    for g in str:gmatch( "[^"..chr.."]" ) do
 	s = s .. g
    end
    return s
end

print( stripchars( "She was a soul stripper. She took my heart!", "aei" ) )
