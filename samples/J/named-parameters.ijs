NB.  Strand notation
myFunc['c:\file.txt'  906  'blue' fs]

NB.  Commas, like other langs
myFunc['c:\file.txt', 906, 'blue' fs]

NB.  Unspecified args are defaulted ("optional")
myFunc['c:\file.txt' fs]

NB.  Can use named arguments, like eg VB
myFunc[color='blue'  fs]

NB.  Often values needn't be quoted
myFunc[color= blue   fs]

NB.  Combination of comma syntax and name=value
myFunc[max=906, color=blue fs]

NB.  Spelling of names is flexible
myFunc[MAX=906, COLOR=blue fs]

NB.  Order of names is flexible
myFunc[COLOR=blue, MAX=906  fs]

NB.  Even the delimiters are flexible...
myFunc<MAX=906, COLOR=blue fs>
