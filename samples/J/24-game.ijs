require'misc'
deal=: 1 + ? bind 9 9 9 9
rules=: smoutput bind 'see http://en.wikipedia.org/wiki/24_Game'
input=: prompt @ ('enter 24 expression using ', ":, ': '"_)

wellformed=: (' '<;._1@, ":@[) -:&(/:~)  '(+-*%)' -.&;:~ ]
is24=: 24 -: ". ::0:@]

respond=: (;:'no yes') {::~ wellformed * is24

game24=: (respond input)@deal@rules
