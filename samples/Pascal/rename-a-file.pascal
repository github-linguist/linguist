  var
    f : file ; // Untyped file
 begin

  // as current directory
  AssignFile(f,'input.doc');
  Rename(f,'output.doc');

  // as root directory
  AssignFile(f,'\input.doc');
  Rename(f,'\output.doc');

  // rename a directory
  AssignFile(f,'docs');
  Rename(f,'mydocs');

  //rename a directory off the root

  AssignFile(f,'\docs');
  Rename(f,'\mydocs');

end;
