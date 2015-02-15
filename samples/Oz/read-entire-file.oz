declare
  FileHandle = {New Open.file init(name:"test.txt")}
  FileContents = {FileHandle read(size:all list:$)}
in
  {FileHandle close}
  {System.printInfo FileContents}
