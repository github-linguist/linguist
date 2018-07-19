Object subclass:'HVar'
    instanceVariableNames:'values'
    classVariableNames:''
    poolDictionaries:''
    category:'example'.
!

!HVar methodsFor:'accessing'!

<-- value
    (values ifNil:[values := OrderedCollection new]) add:value.
    ^  value
!

value
    ^ values last
!

undo
    values removeLast.
!

history
    ^ history
! !

|x|

x := HVar new.
x <-- 1.
x value.
x <-- 2.
x <-- (x value + 1).
x value.
x history.
