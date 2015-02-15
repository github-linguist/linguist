perm=: (A.&i.~ !) 4
ops=: ' ',.'+-*%' {~ >,{i.each 4 4 4
cmask=: 1 + 0j1 * i.@{:@$@[ e. ]
left=:  [ #!.'('~"1 cmask
right=: [ #!.')'~"1 cmask
paren=: 2 :'[: left&m right&n'
parens=: ], 0 paren 3, 0 paren 5, 2 paren 5, [: 0 paren 7 (0 paren 3)
all=: [: parens [:,/ ops ,@,."1/ perm { [:;":each
answer=: ({.@#~ 24 = ".)@all
