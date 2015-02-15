Needs["NETLink`"];
externalFloor = DefineDLLFunction["floor", "msvcrt.dll", "double", { "double" }];
externalFloor[4.2]
-> 4.
