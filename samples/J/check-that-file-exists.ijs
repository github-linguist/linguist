require 'files'
fexist 'input.txt'
fexist '/input.txt'
direxist=: 2 = ftype
direxist 'docs'
direxist '/docs'
