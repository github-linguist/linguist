/*
MIT License

Copyright (c) 2024 koka-community

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
fun list/show(l: list<a>, ?show: (a) -> string): string
  "[" ++ l.map(show).join(", ") ++ "]" 

fun tuple/show(l: (a,b), ?a/show: (a) -> string, ?b/show: (b) -> string): string
  match l
    (a, b) -> "(" ++ a.show ++ ", " ++ b.show ++ ")"

// A special show for characters
//   When resolving an implicit parameter `?show` the
// `chars/show` will be preferred over `list/show` as the chain is shorter
fun chars/show( cs : list<char> ) : string
  "\"" ++ cs.map(string).join ++ "\""

l2.show
// will be inferred as
l2.list/show(?show=fn(l) l.list/show(?show=int/show))
// with all of the implicit parameters eta expanded and filled in explicitly.

fun nl/show(l: list<a>, ?show: (a) -> string): string
  l.map(show).join("\n")

fun padded/show(i: int): string
  if i < 10 then "0" ++ i.show else i.show

value struct paddedlist {inner : list<int>}
fun padded/show(pl: paddedlist): string
  pl.inner.show(?show=padded/show)

named effect file
  fun read-line() : string   // `:(file)   -> <exn> a`
// a (named) handler instance for files
fun file(fname, action) 
  var content := read-text-file(fname.path).lines
  with f <- named handler 
    fun read-line() 
      match content  
        Nil -> "" 
        Cons(x,xx) -> { content := xx; x }
  action(f)
pub fun main() 
  with f1 <- file("package.yaml")
  with f2 <- file("stack.yaml")
  println( f1.read-line() ++ "\n" ++ f2.read-line() )

fun wrong-escape1() 
  with f <- file("stack.yaml")
  f
pub fun test() 
  val f = wrong-escape1()
  f.read-line.println

named scoped effect file<s::S> 
  fun read-line() : string  // `: (f : file<s>) -> scope<s> string`
// a handler instance for files
fun file2(fname : string, action : forall<s> file<s> -> <scope<s>|e> a ) : e a 
  var i := 0
  with f <- named handler 
    fun read-line()
      i := i + 1
      (fname ++ ": line " ++ i.show)
  action(f)

open type mytype
  Constr1(a: int)
  Constr2(b: string)

extend type mytype
  Constr3(c: float64)

alias i-to-s = int -> string
alias i-to-s-e<e> = int -> e string

alias pure = <div,exn>
alias st<h> = <alloc<h>,read<h>,write<h>>

fun sum(xs: list<int>): int
  match xs
    Nil -> 0
    Cons(x, xs) -> x + sum(xs)

// BEGIN:sum-first-two
fun sum-first-two(xs: list<int>): list<int>
  match xs
    Cons(x, tail as Cons(y, _)) -> Cons(x + y, tail)
    xs' -> xs'
// END:sum-first-two

// BEGIN:gather-positives
fun gather-positives(xs: list<int>): list<int>
  match xs
    Nil -> Nil
    Cons(x, xs) | x > 0 -> Cons(x, gather-positives(xs))
    Cons(_, xs) -> gather-positives(xs)
// END:gather-positives

// BEGIN:sum-first-two2
fun sum-first-two2(xs: list<int>): list<int>
  match xs
    Cons(x, tail as Cons(y, _)) -> Cons(x + y, tail)
    xs' -> xs'
// END:sum-first-two2

// BEGIN:gather-positives2
fun gather-positives2(xs: list<int>): list<int>
  match xs
    Nil -> Nil
    Cons(x, xs) | x > 0 -> Cons(x, gather-positives2(xs))
    Cons(_, xs) -> gather-positives2(xs)
// END:gather-positives2

pub effect fail<b>
  final ctl fail(info: b) : a

linear effect pretty
  val indentation: int
  fun print(s: string): ()

extern sizeofint(): int32
  c inline "sizeof(int)"
  js inline "4"

extern add(x: int32, y: int32): int32
  c inline "($1 + $2)"
  js inline "$1 + $2"

extern add2(x: int32, y: int32): int32
  c "add"
  js "add"

extern kk_add(x: int32, y: int32): int32
  c "kk_add"
  js "kk_add"

extern import
  c file "my_file.h"
  js file "my_file.js"

extern import
  c file "my_file"

extern kk_closure(f: () -> int32): int32
  c "kk_closure"

type tree
  Tip
  Bin( left: tree, value : int, right: tree )

fbip fun tmap-inorder( t : tree, f : int -> int ) : tree
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

fip fun tmap( f : int -> int, t : tree, visit : visitor, d : direction )
  match d
    Down -> match t     // going down a left spine
      Bin(l,x,r) -> tmap(f,l,BinR(r,x,visit),Down) // A
      Tip        -> tmap(f,Tip,visit,Up)           // B
    Up -> match visit   // go up through the visitor
      Done        -> t                             // C
      BinR(r,x,v) -> tmap(f,r,BinL(t,f(x),v),Down) // D
      BinL(l,x,v) -> tmap(f,Bin(l,x,t),v,Up)       // E