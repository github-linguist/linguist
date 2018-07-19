$ gst
GNU Smalltalk ready

st> |concat|
st> concat := [ :a :b :c | (a,c,c,b) displayNl ].
a BlockClosure
st> concat value: 'Rosetta' value: 'Code' value: ':'.
Rosetta::Code
'Rosetta::Code'
st>
