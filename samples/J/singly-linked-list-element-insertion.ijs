list=: 1 65,:_ 66
A=:0  NB. reference into list
B=:1  NB. reference into list
insertAfter=: monad define
   'localListName localListNode localNewValue'=. y
   localListValue=: ".localListName
   localOldLinkRef=: <localListNode,0
   localNewLinkRef=: #localListValue
   localNewNode=: (localOldLinkRef { localListValue), localNewValue
   (localListName)=: (localNewLinkRef localOldLinkRef} localListValue), localNewNode
)
