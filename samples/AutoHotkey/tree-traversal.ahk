AddNode(Tree,1,2,3,1) ; Build global Tree
AddNode(Tree,2,4,5,2)
AddNode(Tree,3,6,0,3)
AddNode(Tree,4,7,0,4)
AddNode(Tree,5,0,0,5)
AddNode(Tree,6,8,9,6)
AddNode(Tree,7,0,0,7)
AddNode(Tree,8,0,0,8)
AddNode(Tree,9,0,0,9)

MsgBox % "Preorder: "   PreOrder(Tree,1)  ; 1 2 4 7 5 3 6 8 9
MsgBox % "Inorder: "    InOrder(Tree,1)   ; 7 4 2 5 1 8 6 9 3
MsgBox % "postorder: "  PostOrder(Tree,1) ; 7 4 5 2 8 9 6 3 1
MsgBox % "levelorder: " LevOrder(Tree,1)  ; 1 2 3 4 5 6 7 8 9

AddNode(ByRef Tree,Node,Left,Right,Value) {
   if !isobject(Tree)
     Tree := object()

   Tree[Node, "L"] := Left
   Tree[Node, "R"] := Right
   Tree[Node, "V"] := Value
}

PreOrder(Tree,Node) {
ptree := Tree[Node, "V"] " "
        . ((L:=Tree[Node, "L"]) ? PreOrder(Tree,L) : "")
        . ((R:=Tree[Node, "R"]) ? PreOrder(Tree,R) : "")
return ptree
}
InOrder(Tree,Node) {
   Return itree := ((L:=Tree[Node, "L"]) ? InOrder(Tree,L) : "")
        . Tree[Node, "V"] " "
        . ((R:=Tree[Node, "R"]) ? InOrder(Tree,R) : "")
}
PostOrder(Tree,Node) {
   Return ptree := ((L:=Tree[Node, "L"]) ? PostOrder(Tree,L) : "")
        . ((R:=Tree[Node, "R"]) ? PostOrder(Tree,R) : "")
        . Tree[Node, "V"] " "
}
LevOrder(Tree,Node,Lev=1) {
   Static                        ; make node lists static
   i%Lev% .= Tree[Node, "V"] " " ; build node lists in every level
   If (L:=Tree[Node, "L"])
       LevOrder(Tree,L,Lev+1)
   If (R:=Tree[Node, "R"])
       LevOrder(Tree,R,Lev+1)
   If (Lev > 1)
      Return
   While i%Lev%                  ; concatenate node lists from all levels
      t .= i%Lev%, Lev++
   Return t
}
