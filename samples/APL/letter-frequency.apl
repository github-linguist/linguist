      freq←{(⍪∪⍵),+/(∪⍵)∘.⍷⍵}

      freq 0 1 2 3 2 3 4 3 4 4 4
0 1
1 1
2 2
3 3
4 4

      freq 'balloon'
b 1
a 1
l 2
o 2
n 1
