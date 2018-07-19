Truncate[file_, n_] := Module[{filename = file, nbbytes = n, temp},
  temp = $TemporaryPrefix <> filename;
  BinaryWrite[temp, BinaryReadList[filename, "Byte", nbbytes]];
  Close[temp]; DeleteFile[filename]; RenameFile[temp, filename];
  ]
