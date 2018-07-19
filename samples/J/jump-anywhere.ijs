F=: verb define
  smoutput 'Now we are in F'
  G''
  smoutput 'Now we are back in F'
)

G=: verb define
  smoutput 'Now we are in G'
  throw.
)

   F''
Now we are in F
Now we are in G
