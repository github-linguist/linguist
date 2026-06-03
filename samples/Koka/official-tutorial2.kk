/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

effect val width : int

fun pretty-internal( line : string ) : width string
  line.truncate(width)

fun pretty-thin(d : doc) : string
  with val width = 40
  pretty(d)

effect fun emit( msg : string ) : ()

fun ehello() : emit ()
  emit("hello")
  emit("world")

fun ehello-console() : console ()
  with fun emit(msg) println(msg)
  ehello()

fun emit-console( action )
  with fun emit(msg) println(msg)
  action()

val emit-console2 = handler
  fun emit(msg) println(msg)

fun ehello-console2() : console ()
  with emit-console
  ehello()

fun emit-collect( action : () -> <emit|e> () ) : e string
  var lines := []
  with handler
    return(x)     lines.reverse.join("\n")
    fun emit(msg) lines := Cons(msg,lines)
  action()

fun ehello-commit() : string
  with emit-collect
  ehello()

fun catch( hnd : (string) -> e a, action : () -> <raise|e> a ) : e a
  with ctl raise(msg) hnd(msg)
  action()

fun catch-example()
  with catch( fn(msg){ println("error: " ++ msg); 42 } )
  safe-divide(1,0)

fun raise-maybe( action : () -> <raise|e> a ) : e maybe<a>
  with handler
    return(x)      Just(x)   // normal return: wrap in Just
    ctl raise(msg) Nothing   // exception: return Nothing directly
  action()

fun div42()
  (raise-maybe{ safe-divide(1,0) }).default(42)

effect state<a>
  fun get() : a
  fun set( x : a ) : ()

fun sumdown( sum : int = 0 ) : <state<int>,div> int
  val i = get()
  if i <= 0 then sum else
    set( i - 1 )
    sumdown( sum + i )

fun state( init : a, action : () -> <state<a>,div|e> b ) : <div|e> b
  var st := init
  with handler
    fun get()  st
    fun set(i) st := i
  action()

fun pstate( init : a, action : () -> <state<a>,div|e> b ) : <div|e> (b,a)
  var st := init
  with handler
    return(x)  (x,st)       // pair with the final state
    fun get()  st
    fun set(i) st := i
  action()

fun pstate2( init : a, action : () -> <state<a>,div|e> b ) : <div|e> (b,a)
  var st := init
  with return(x) (x,st)
  with handler
    fun get()  st
    fun set(i) st := i
  action()

fun no-odds() : <raise,state<int>> int
  val i = get()
  if i.is-odd then raise("no odds") else
    set(i / 2)
    i

fun state-raise(init) : div (maybe<int>,int)
  with pstate(init)
  with raise-maybe
  no-odds()

fun raise-state(init) : div maybe<(int,int)>
  with raise-maybe
  with pstate(init)
  no-odds()

fun mask-emit()
  with fun emit(msg) println("outer:" ++ msg)
  with fun emit(msg) println("inner:" ++ msg)
  emit("hi")
  mask<emit>
    emit("there")

fun mask-print( action : () -> e int ) : e int
  with ctl raise(msg) 42
  val x = mask<raise>(action)
  if x.is-odd then raise("wrong")   // internal exception
  x

effect<a> val peek : a             // get the state
effect<a> ctl poke( x : a ) : ()   // set the state to x

fun ppstate( init : a, action : () -> <peek<a>,poke<a>|e> b ) : e b
  with val peek = init
  with ctl poke(x)
    mask<peek>
      with val peek = x
      resume(())
  action()

fun emit-quoted1( action : () -> <emit,emit|e> a ) : <emit|e> a
  with fun emit(msg) emit("\"" ++ msg ++ "\"")
  action()

fun emit-quoted2( action : () -> <emit|e> a ) : <emit|e> a
  with override fun emit(msg) emit("\"" ++ msg ++ "\"" )
  action()

fun extra-wide( action )
  with override val width = 2*width
  action()

effect ctl choice() : bool

fun xor() : choice bool
  val p = choice()
  val q = choice()
  if p then !q else q

fun choice-random(action : () -> <choice,random|e> a) : <random|e> a
  with fun choice() random-bool()
  action()

fun choice-all(action : () -> <choice|e> a) : e list<a>
  with handler
    return(x)    [x]
    ctl choice() resume(False) ++ resume(True)
  action()

fun surprising() : <choice,state<int>> bool
  val p = choice()
  val i = get()
  set(i+1)
  if i>0 && p then xor() else False

fun state-choice() : div (list<bool>,int)
  pstate(0)
    choice-all(surprising)

fun choice-state() : div list<(bool,int)>
  choice-all
    pstate(0,surprising)

type fhandle
fun fopen( path : string )   : <exn,filesys> fhandle
fun hreadline( h : fhandle ) : <exn,filesys> string
fun hclose( h : fhandle )    : <exn,filesys> ()

effect fun fread() : string

fun with-file( path : string, action : () -> <fread,exn,filesys|e> a ) : <exn,filesys|e> a
  val h = fopen(path)
  with handler
    return(x)   { hclose(h); x }
    fun fread() hreadline(h)
  action()

fun with-file( path : string, action : () -> <fread,exn,filesys|e> a ) : <exn,filesys|e> a
  val h = fopen(path)
  with finally
    hclose(h)
  with fun fread()
    hreadline(h)
  action()

type tree
  Tip
  Bin( left: tree, value : int, right: tree )

fun tmap-inorder( t : tree, f : int -> int ) : tree
  match t
    Bin(l,x,r) -> Bin( l.tmap-inorder(f), f(x), r.tmap-inorder(f) )
    Tip        -> Tip

type visitor
  Done
  BinR( right:tree, value : int, visit : visitor )
  BinL( left:tree, value : int, visit : visitor )

type direction
  Up
  Down

fun tmap( f : int -> int, t : tree, visit : visitor, d : direction )
  match d
    Down -> match t     // going down a left spine
      Bin(l,x,r) -> tmap(f,l,BinR(r,x,visit),Down) // A
      Tip        -> tmap(f,Tip,visit,Up)           // B
    Up -> match visit   // go up through the visitor
      Done        -> t                             // C
      BinR(r,x,v) -> tmap(f,r,BinL(t,f(x),v),Down) // D
      BinL(l,x,v) -> tmap(f,Bin(l,x,t),v,Up)       // E
