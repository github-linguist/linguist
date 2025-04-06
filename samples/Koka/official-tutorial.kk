/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/
import std/num/float64

fun main() {
  println("Hello world!") // println output
}

fun encode( s : string, shift : int )
  fun encode-char(c)
    if c < 'a' || c > 'z' then return c
    val base = (c - 'a').int
    val rot  = (base + shift) % 26
    (rot.char + 'a')
  s.map(encode-char)

fun caesar( s : string ) : string
  s.encode( 3 )

fun showit( s : string )
  s.encode(3).count.println

fun encode2( s : string, shift : int )
  s.map( fn(c)
    if c < 'a' || c > 'z' then return c
    val base = (c - 'a').int
    val rot  = (base + shift) % 26
    (rot.char + 'a')
  )

fun print10()
  for(1,10) fn(i)
    println(i)

fun printhi10()
  repeat(10)
    println("hi")

fun print11()
  var i := 10
  while { i >= 0 }
    println(i)
    i := i - 1

fun twice(f)
  f()
  f()

fun test-twice()
  twice
    twice
      println("hi")

pub fun test-with1()
  with twice
  with twice
  println("hi")

pub fun test-with2() {
  with x <- list(1,10).foreach
  println(x)
}

fun test-finally()
  with finally{ println("exiting..") }
  println("entering..")
  throw("oops") + 42

// declare an abstract operation: emit, how it emits is defined dynamically by a handler.
effect fun emit(msg : string) : ()

// emit a standard greeting.
fun hello() : emit ()
  emit("hello world!")

// emit a standard greeting to the console.
pub fun hello-console1() : console ()
  with handler
    fun emit(msg) println(msg)
  hello()

fun world()
  replace-all("hi there", "there", "world")  // returns "hi world"

fun world2()
  "hi there".replace-all( repl="world", pattern="there" )

fun sublist( xs : list<a>, start : int, len : int = xs.length ) : list<a>
  if start <= 0 return xs.take(len)
  match xs
    Nil        -> Nil
    Cons(_,xx) -> xx.sublist(start - 1, len)

// The letter frequency table for English
val english = [8.2,1.5,2.8,4.3,12.7,2.2,
               2.0,6.1,7.0,0.2,0.8,4.0,2.4,
               6.7,7.5,1.9,0.1, 6.0,6.3,9.1,
               2.8,1.0,2.4,0.2,2.0,0.1]

// Small helper functions
fun percent( n : int, m : int )
  100.0 * (n.float64 / m.float64)

fun rotate( xs : list<a>, n : int ) : list<a>
  xs.drop(n) ++ xs.take(n)

// Calculate a frequency table for a string
fun freqs( s : string ) : list<float64>
  val lowers = list('a','z')
  val occurs = lowers.map( fn(c) s.count(c.string) )
  val total  = occurs.sum
  occurs.map( fn(i) percent(i,total) )

// Calculate how well two frequency tables match according
// to the _chi-square_ statistic.
fun chisqr( xs : list<float64>, ys : list<float64> ) : float64
  zipwith(xs,ys, fn(x,y) ((x - y)^2.0)/y ).foldr(0.0,(+))

// Crack a Caesar encoded string
fun uncaesar( s : string ) : string
  val table  = freqs(s)                   // build a frequency table for `s`
  val chitab = list(0,25).map fn(n)       // build a list of chisqr numbers for each shift between 0 and 25
                 chisqr( table.rotate(n), english )

  val min    = chitab.minimum()           // find the mininal element
  val shift  = chitab.index-of( fn(f) f == min ).negate  // and use its position as our shift
  s.encode( shift )

fun test-uncaesar()
  println( uncaesar( "nrnd lv d ixq odqjxdjh" ) )

fun square1( x : int ) : total int   { x*x }
fun square2( x : int ) : console int { println( "a not so secret side-effect" ); x*x }
fun square3( x : int ) : div int     { x * square3( x ) }
fun square4( x : int ) : exn int     { throw( "oops" ); x*x }

fun square5( x : int ) : int
  x*x

fun square6( x : int ) : _e int
  println("I did not want to write down the \"console\" effect")
  x*x

fun combine-effects()
  val i = srandom-int() // non-deterministic
  throw("oops")         // exception raising
  combine-effects()     // and non-terminating

alias pure = <div,exn>

fun map : (xs : list<a>, f : (a) -> e b) -> e list<b>
  match xs
    Cons(x,xx) -> Cons(f(x), xx.map(f))
    Nil -> Nil

fun looptest()
  while { is-odd(srandom-int()) }
    throw("odd")

fun fib(n : int) : div int
  if n <= 0   then 0
  elif n == 1 then 1
  else fib(n - 1) + fib(n - 2)

fun fib2(n)
  var x := 0
  var y := 1
  repeat(n)
    val y0 = y
    y := x+y
    x := y0
  x

fun wrong() : (() -> console ())
  var x := 1
  (fn(){ x := x + 1; println(x) })

fun fib3(n)
  val x = ref(0)
  val y = ref(1)
  repeat(n)
    val y0 = !y
    y := !x + !y
    x := y0
  !x

struct person
  age : int
  name : string
  realname : string = name

val brian = Person( 29, "Brian" )

fun birthday( p : person ) : person
  p( age = p.age + 1 )

fun copy( p, age = p.age, name = p.name, realname = p.realname )
  Person(age, name, realname)

type color
  Red
  Green
  Blue

type void

type ()
  ()

type bool
  False
  True

type number
  Infinity
  Integer( i : int )

type list<a>
  Nil
  Cons{ head : a; tail : list<a> }

struct person2{ age : int; name : string; realname : string = name }

type person3
  Person{ age : int; name : string; realname : string = name }

type person4
  Person
    age : int
    name : string
    realname : string = name

value struct argb{ alpha: int; color-red: int; color-green: int; color-blue: int }

effect raise
  ctl raise( msg : string ) : a

fun safe-divide( x : int, y : int ) : raise int
  if y==0 then raise("div-by-zero") else x / y

fun raise-const() : int
  with handler
    ctl raise(msg) 42
  8 + safe-divide(1,0)

fun raise-const1() : int
  with ctl raise(msg) 42
  8 + safe-divide(1,0)

effect ask<a>                   // or: effect<a> ctl ask() : a
  ctl ask() : a

fun add-twice() : ask<int> int
  ask() + ask()

fun ask-const() : int
  with ctl ask() resume(21)
  add-twice()

fun ask-random() : random int
  with ctl ask() resume(random-int())
  add-twice()

fun ask-once() : int
  var count := 0
  with ctl ask()
    count := count + 1
    if count <= 1 then resume(42) else 0
  add-twice()

fun ask-const2() : int
  with fun ask() 21
  add-twice()

effect ask2<a>
  fun ask() : a

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