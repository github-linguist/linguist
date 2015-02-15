function Count_Substring( s1, s2 )
 local magic =  "[%^%$%(%)%%%.%[%]%*%+%-%?]"
 local percent = function(s)return "%"..s end
    return select( 2, s1:gsub( s2:gsub(magic,percent), "" ) )
end

print( Count_Substring( "the three truths", "th" ) )
print( Count_Substring( "ababababab","abab" ) )
