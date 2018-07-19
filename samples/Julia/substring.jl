julia> s = "abcdefg"
"abcdefg"

julia> n = 3
3

julia> s[n:end]
"cdefg"

julia> m=2
2

julia> s[n:n+m]
"cde"

julia> s[1:end-1]
"abcdef"

julia> s[search(s,'c')]
'c'

julia> s[search(s,'c'):search(s,'c')+m]
"cde"
