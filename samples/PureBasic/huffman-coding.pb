OpenConsole()

SampleString.s="this is an example for huffman encoding"
datalen=Len(SampleString)

Structure ztree
  linked.c
  ischar.c
  char.c
  number.l
  left.l
  right.l
EndStructure

Dim memc.c(0)
memc()=@SampleString

Dim tree.ztree(255)

For i=0 To datalen-1
  tree(memc(i))\char=memc(i)
  tree(memc(i))\number+1
  tree(memc(i))\ischar=1
Next

SortStructuredArray(tree(),#PB_Sort_Descending,OffsetOf(ztree\number),#PB_Sort_Character)

For i=0 To 255
  If tree(i)\number=0
    ReDim tree(i-1)
    Break
  EndIf
Next

dimsize=ArraySize(tree())
Repeat
  min1.l=0
  min2.l=0
  For i=0 To dimsize
    If tree(i)\linked=0
      If tree(i)\number<min1 Or min1=0
        min1=tree(i)\number
        hmin1=i
      ElseIf tree(i)\number<min2 Or min2=0
        min2=tree(i)\number
        hmin2=i
      EndIf
    EndIf
  Next

  If min1=0 Or min2=0
    Break
  EndIf

  dimsize+1
  ReDim tree(dimsize)
  tree(dimsize)\number=tree(hmin1)\number+tree(hmin2)\number
  tree(hmin1)\left=dimsize
  tree(hmin2)\right=dimsize
  tree(hmin1)\linked=1
  tree(hmin2)\linked=1
ForEver

i=0
While tree(i)\ischar=1
  str.s=""
  k=i
  ZNEXT:
  If tree(k)\left<>0
    str="0"+str
    k=tree(k)\left
    Goto ZNEXT
  ElseIf tree(k)\right<>0
    str="1"+str
    k=tree(k)\right
    Goto ZNEXT
  EndIf
  PrintN(Chr(tree(i)\char)+" "+str)
  i+1
Wend
Input()

CloseConsole()
