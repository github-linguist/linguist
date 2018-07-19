SetDirectory@NotebookDirectory[];
t = OpenWrite["output.txt"]
Close[t]
s = OpenWrite[First@FileNameSplit[$InstallationDirectory] <> "\\output.txt"]
Close[s]

(*In root directory*)
CreateDirectory["\\docs"]
(*In current operating directory*)
CreateDirectory[Directory[]<>"\\docs"]
(* "<>" is shorthand for "StringJoin[left,right]" *)
