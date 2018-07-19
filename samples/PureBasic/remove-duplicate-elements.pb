NewMap MyElements.s()

For i=0 To 9              ;Mark 10 items at random, causing high risk of duplication items.
  x=Random(9)
  t$="Number "+str(x)+" is marked"
  MyElements(str(x))=t$   ; Add element 'X' to the hash list or overwrite if already included.
Next

ForEach MyElements()
  Debug MyElements()
Next
