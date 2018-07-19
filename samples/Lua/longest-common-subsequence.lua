function LCS( a, b )
    if #a == 0 or #b == 0 then
        return ""
    elseif string.sub( a, -1, -1 ) == string.sub( b, -1, -1 ) then
        return LCS( string.sub( a, 1, -2 ), string.sub( b, 1, -2 ) ) .. string.sub( a, -1, -1 )
    else
        local a_sub = LCS( a, string.sub( b, 1, -2 ) )
        local b_sub = LCS( string.sub( a, 1, -2 ), b )

        if #a_sub > #b_sub then
            return a_sub
        else
            return b_sub
        end
    end
end

print( LCS( "thisisatest", "testing123testing" ) )
