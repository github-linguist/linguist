module fileio ;
import std.stdio ;
import std.path ;
import std.file ;
import std.stream ;

string[] genName(string name){
  string cwd  = curdir ~ sep ; // on current directory
  string root = sep ;          // on root
  name = std.path.getBaseName(name) ;
  return [cwd ~ name, root ~ name] ;
}
void Remove(string target){
  if(exists(target)){
    if (isfile(target))
      std.file.remove(target);
    else
      std.file.rmdir(target) ;
  }
}
void testCreate(string filename, string dirname){
  // files:
  foreach(fn ; genName(filename))
    try{
      writefln("file to be created : %s", fn) ;
      std.file.write(fn, cast(void[])null) ;
      writefln("\tsuccess by std.file.write") ; Remove(fn) ;
      (new std.stream.File(fn, FileMode.OutNew)).close() ;
      writefln("\tsuccess by std.stream") ; Remove(fn) ;
    } catch(Exception e) {
      writefln(e.msg) ;
    }
  // dirs:
  foreach(dn ; genName(dirname))
    try{
      writefln("dir to be created : %s", dn) ;
      std.file.mkdir(dn) ;
      writefln("\tsuccess by std.file.mkdir") ; Remove(dn) ;
    } catch(Exception e) {
      writefln(e.msg) ;
    }
}
void main(){
  writefln("== test: File & Dir Creation ==") ;
  testCreate("output.txt", "docs") ;
}
