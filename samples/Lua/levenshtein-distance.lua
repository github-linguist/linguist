function Levenshtein_Distance( s1, s2 )
    if s1:len() == 0 then return s2:len() end
    if s2:len() == 0 then return s1:len() end

	if s1:sub( -1, -1 ) == s2:sub( -1, -1 ) then
	    return Levenshtein_Distance( s1:sub( 1, -2 ), s2:sub( 1, -2 ) )
	end

	local a = Levenshtein_Distance( s1:sub( 1, -2 ), s2:sub( 1, -2 ) )
	local b = Levenshtein_Distance( s1:sub( 1, -1 ), s2:sub( 1, -2 ) )
	local c = Levenshtein_Distance( s1:sub( 1, -2 ), s2:sub( 1, -1 ) )

	if a > b then return b + 1 end
	if a > c then return c + 1 end
	return a + 1
end

print( Levenshtein_Distance( "kitten", "sitting" ) )
print( Levenshtein_Distance( "rosettacode", "raisethysword" ) )
