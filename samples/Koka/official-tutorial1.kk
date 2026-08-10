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
