module singleton ;
import std.stdio ;
import std.thread ;
import std.random ;
import std.c.time ;

class Dealer {
  private static Dealer me ;
  static Dealer Instance() {
    writefln("   Calling Dealer... ") ;
    if(me is null) // Double Checked Lock
      synchronized  // this part of code can only be executed by one thread a time
        if(me is null)
          me = new Dealer ;
    return me ;
  }
  private static string[] str = ["(1)Enjoy", "(2)Rosetta", "(3)Code"] ;
  private int state ;
  private this() {
    for(int i = 0 ; i < 3 ; i++) {
      writefln("...calling Dealer... ") ;
      msleep(rand() & 2047) ;
    }
    writefln(">>Dealer is called to come in!") ;
    state = str.length - 1 ;
  }
  Dealer nextState() {
    synchronized(this) // accessed to Object _this_ is locked ... is it necessary ???
      state = (state + 1) % str.length ;
    return this ;
  }
  string toString() { return str[state] ; }
}

class Coder : Thread {
  private string name_ ;
  Coder hasName(string name) {  name_ = name ; return this ; }
  override int run() {
    msleep(rand() & 1023) ;
    writefln(">>%s come in.", name_) ;
    Dealer single = Dealer.Instance ;
    msleep(rand() & 1023) ;
    for(int i = 0 ; i < 3 ; i++) {
      writefln("%9s got %-s", name_, single.nextState) ;
      msleep(rand() & 1023) ;
    }
    return 0 ;
  }
}

void main() {
  Coder x = new Coder ;
  Coder y = new Coder ;
  Coder z = new Coder ;

  x.hasName("Peter").start() ;
  y.hasName("Paul").start() ;
  z.hasName("Mary").start() ;

  x.wait ;  y.wait ;  z.wait ;
}
