cypher[mesg_String,n_Integer]:=StringReplace[mesg,Flatten[Thread[Rule[#,RotateLeft[#,3]]]&/@CharacterRange@@@{{"a","z"},{"A","Z"}}]]
