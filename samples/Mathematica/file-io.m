SetDirectory@NotebookDirectory[];
If[FileExistsQ["output.txt"], DeleteFile["output.txt"], Print["No output yet"] ];
CopyFile["input.txt", "output.txt"]
