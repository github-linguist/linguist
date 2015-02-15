Dim keys.s(3)
Dim vals.s(3)
NewMap Hash.s()

keys(0)="a" : keys(1)="b" : keys(2)="c" : keys(3)="d"
vals(0)="1" : vals(1)="2" : vals(2)="3" : vals(3)="4"
For n = 0 To 3
    Hash(keys(n))= vals(n)
Next
ForEach Hash()
   Debug Hash()
Next
