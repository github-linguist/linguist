Needs["NETLink`"];
externalstrdup = DefineDLLFunction["_strdup", "msvcrt.dll", "string", {"string"}];
Print["Duplicate: ", externalstrdup["Hello world!"]]
