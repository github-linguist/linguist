julia> expr="2 * (3 -1) + 2 * 5"
"2 * (3 -1) + 2 * 5"

julia> parsed = parse(expr) #Julia provides low-level access to language parser for AST/Expr creation
:(+(*(2,-(3,1)),*(2,5)))

julia> t = typeof(parsed)
Expr

julia> names(t) #shows type fields
(:head,:args,:typ)

julia> parsed.args #Inspect our 'Expr' type innards
3-element Any Array:
 :+
 :(*(2,-(3,1)))
 :(*(2,5))

julia> typeof(parsed.args[2]) #'Expr' types can nest
Expr

julia> parsed.args[2].args
3-element Any Array:
  :*
 2
  :(-(3,1))

julia> parsed.args[2].args[3].args #Will nest until lowest level of AST
3-element Any Array:
  :-
 3
 1

julia> eval(parsed)
14

julia> eval(parse("1 - 5 * 2 / 20 + 1"))
1.5

julia> eval(parse("2 * (3 + ((5) / (7 - 11)))"))
3.5
