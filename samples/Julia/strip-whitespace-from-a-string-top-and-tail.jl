julia> s = " \t \r \n String with spaces  \t  \r  \n  "
" \t \r \n String with spaces  \t  \r  \n  "

julia> lstrip(s)
"String with spaces  \t  \r  \n  "

julia> rstrip(s)
" \t \r \n String with spaces"

julia> strip(s)
"String with spaces"
