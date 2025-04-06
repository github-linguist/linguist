/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard `:list` functions.
module std/core/list

import std/core/types
import std/core/undiv
import std/core/hnd
import std/core/exn
import std/core/char
import std/core/string
import std/core/int
import std/core/vector


// Return the head of list if the list is not empty.
pub fbip fun head( xs : list<a> ) : maybe<a>
  match xs
    Cons(x) -> Just(x)
    _       -> Nothing

// Return the head of list with a default value in case the list is empty.
pub fbip fun default/head( xs : list<a>, default : a ) : a
  match xs
    Cons(x) -> x
    _       -> default

// Return the tail of list. Returns the empty list if `xs` is empty.
pub fbip fun tail( xs : list<a> ) : list<a>
  match xs
    Cons(_,xx) -> xx
    _          -> []

// Is the list empty?
pub fip fun is-empty( ^xs : list<a> ) : bool
  match xs
    Nil -> True
    _   -> False


// ----------------------------------------------------------------------------
// List functions
// ----------------------------------------------------------------------------

// Returns a singleton list.
pub fip(1) fun single(x)
  [x]

// Returns the length of a list.
pub fun length(xs)
  fun len(ys,acc)
    match ys
      Cons(_,yy) -> yy.len(acc+1)
      Nil        -> acc
  xs.len(0)

// Returns an integer list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
pub fun list( lo: int, hi: int ) : total list<int>
  if lo <= hi
    then Cons( lo, list( pretend-decreasing(lo.inc), hi ) )
    else Nil

// Returns an integer list of increasing elements from `lo`  to `hi` with stride `stride`.
// If `lo > hi`  the function returns the empty list.
pub fun stride/list( lo: int, hi: int, stride : int ) : total list<int>
  if lo <= hi
    then Cons( lo, list(  pretend-decreasing(lo + stride), hi, stride ))
    else Nil

// Applies a function `f` to list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
pub fun function/list( lo: int, hi: int, f : int -> e a ) : e list<a>
  if lo <= hi
    then Cons( f(lo), list( pretend-decreasing(lo.inc), hi, f ))
    else Nil

// Returns an integer list of increasing elements from `lo`  to `hi` with stride `stride`.
// If `lo > hi`  the function returns the empty list.
pub fun stridefunction/list( lo: int, hi: int, stride : int, f : int -> e a ) : e list<a>
  if lo <= hi
    then Cons( f(lo), list(  pretend-decreasing(lo + stride), hi, stride, f ))
    else Nil

// Create a list of characters from `lo`  to `hi`  (including `hi`).
pub fun char/list( lo : char, hi : char ) : total list<char>
  list(lo.int, hi.int).map( char )

// Element-wise list equality
pub fun (==)( xs : list<a>, ys : list<a>, ?(==) : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
                    Nil        -> False
                    Cons(y,yy) -> (x==y) && (xx==yy)
    Nil -> match ys
             Nil  -> True
             Cons -> False

// Order on lists
pub fun cmp( xs : list<a>, ys : list<a>, ?cmp : (a,a) -> order ) : order
  match xs
    Cons(x,xx) -> match ys
                    Nil        -> Gt
                    Cons(y,yy) -> match cmp(x,y)
                                    Eq -> cmp(xx,yy)
                                    lg -> lg
    Nil -> match ys
             Nil  -> Eq
             Cons -> Lt


// Show a list
pub fun show( xs : list<a>, ?show : a -> e string ) : e string
  "[" ++ xs.map(show).join(",") ++ "]"


// _deprecated_, use `list/show` instead.
pub fun show-list( xs : list<a>, show-elem : (a) -> e string ) : e string
  show(xs,?show=show-elem)



// Zip two lists together by pairing the corresponding elements.
// The returned list is only as long as the smallest input list.
pub fbip fun zip( xs : list<a>, ys : list<b> ) : list<(a,b)>
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) -> Cons((x,y),zip(xx,yy))
      Nil        -> Nil
    Nil -> Nil

// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.
pub fbip fun zipwith( xs : list<a>, ys :list<b>, ^f : (a,b) -> e c ) : e list<c>
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) -> Cons(f(x,y),zipwith(xx,yy,f))
      Nil -> Nil
    _ -> Nil

// Zip two lists together by apply a function `f` to all corresponding elements
// and their index in the list.
// The returned list is only as long as the smallest input list.
pub fbip fun zipwith-indexed( xs0 : list<a>, ys0 :list<b>, ^f : (int,a,b) -> e c ) : e list<c>
  zipwith-iter(0,xs0,ys0,f)

fbip fun zipwith-iter( i, xs, ys, ^f )
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) -> Cons(f(i,x,y),zipwith-iter(i+1,xx,yy,f))
      Nil -> Nil
    Nil -> Nil

// Unzip a list of pairs into two lists
pub fun unzip( xs : list<(a,b)> ) : (list<a>,list<b>)
  fun iter( ys, acc1, acc2 )
    match ys
      Cons((x,y),xx) -> iter(xx,acc1 ++ ctx Cons(x,_),acc2 ++ ctx Cons(y,_))
      Nil            -> (acc1 ++. Nil,acc2 ++. Nil)
  iter(xs,ctx _,ctx _)

// Unzip a list of triples into three lists
pub fun unzip3( xs : list<(a,b,c)>) : (list<a>,list<b>,list<c>)
  fun iter( ys, acc1, acc2, acc3 )
    match ys
      Cons((x,y,z),xx) -> iter(xx,acc1 ++ ctx Cons(x,_),acc2 ++ ctx Cons(y,_),acc3 ++ ctx Cons(z,_))
      Nil            -> (acc1 ++. Nil,acc2 ++. Nil,acc3 ++. Nil)
  iter(xs,ctx _,ctx _,ctx _)

// Unzip a list of quadruples into four lists
pub fun unzip4( xs : list<(a,b,c,d)>) : (list<a>,list<b>,list<c>,list<d>)
  fun iter( ys, acc1, acc2, acc3, acc4 )
    match ys
      Cons((x,y,z,w),xx) -> iter(xx,acc1 ++ ctx Cons(x,_),acc2 ++ ctx Cons(y,_),acc3 ++ ctx Cons(z,_),acc4 ++ ctx Cons(w,_))
      Nil            -> (acc1 ++. Nil,acc2 ++. Nil,acc3 ++. Nil,acc4 ++. Nil)
  iter(xs,ctx _,ctx _,ctx _,ctx _)

// Take the first `n` elements of a list (or fewer if the list is shorter than `n`)
pub fun take( xs : list<a>, n : int ) : list<a>
  match xs
    Cons(x,xx) | n > 0 -> Cons(x, take(xx, n - 1))
    _ -> Nil

// Drop the first `n` elements of a list (or fewer if the list is shorter than `n`)
pub fun drop( xs : list<a>, n : int ) : list<a>
  match xs
    Cons(_,xx) | n > 0 -> drop(xx, n - 1)
    _ -> xs

// Apply a function `f` to each element of the input list in sequence.
pub fip fun map(xs : list<a>, ^f : a -> e b) : e list<b>
  match xs
    Cons(x,xx) -> Cons(f(x), xx.map(f))
    Nil -> Nil

// Apply a function `f` to each element of the input list in sequence where takes
// both the index of the current element and the element itself as arguments.
pub fun map-indexed(xs : list<a>, f : (idx : int, value : a) -> e b) : e list<b>
  fun map-idx(ys,i)
    match ys
      Cons(y,yy) -> Cons(f(i,y), map-idx(yy,i+1))
      Nil -> Nil
  map-idx(xs,0)

// Apply a function `f` to each element of the input list in sequence where `f` takes
// both the current element and the tail list as arguments.
pub fun map-peek(xs : list<a>, f : (value : a, rest : list<a>) -> e b) : e list<b>
  fun mappeek(ys)
    match ys
      Cons(y,yy) -> Cons(f(y,yy), yy.mappeek)
      Nil -> Nil
  mappeek(xs)

// Apply a function `f` to each element of the input list in sequence where takes
// both the index of the current element, the element itself, and the tail list as arguments.
pub fun map-indexed-peek(xs : list<a>, f : (idx : int, value : a, rest : list<a> ) -> e b) : e list<b>
  fun mapidx(ys,i)
    match ys
      Cons(y,yy) -> Cons(f(i,y,yy), mapidx(yy,i+1))
      Nil -> Nil
  mapidx(xs,0)

// Reverse a list.
pub fip fun reverse(xs : list<a>) : list<a>
  reverse-append( xs, Nil )

// Efficiently reverse a list `xs` and append it to `tl`:
// `reverse-append(xs,tl) == reverse(xs) ++ tl
pub fip fun reverse-append( xs : list<a>, tl : list<a> ) : list<a>
  reverse-acc(tl,xs)

fip fun reverse-acc(acc : list<a>, ys : list<a> ) : list<a>
    match ys
      Cons(x,xx) -> reverse-acc(Cons(x,acc),xx)
      _          -> acc

// Append two lists.
pub fip fun (++)(xs : list<a>, ys : list<a> ) : list<a>
  append(xs,ys)

// Append two lists.
pub fip fun append(xs : list<a>, ys : list<a> ) : list<a>
  match xs
    Cons(x,xx) -> Cons(x,append(xx,ys))
    Nil -> ys

// Fold a list from the right, i.e. `foldr([1,2],0,(+)) == 1+(2+0)`
// Note, `foldr` is less efficient than `foldl` as it reverses the list first.
pub fun foldr(xs : list<a>, z : b, f : (a, b) -> e b) : e b
  xs.reverse.foldl(z) fn(x,y) f(y,x)

// Fold a list from the left, i.e. `foldl([1,2],0,(+)) == (0+1)+2`
// Since `foldl` is tail recursive, it is preferred over `foldr` when using an associative function `f`
pub fun foldl(xs : list<a>, z : b, f : (b, a) -> e b) : e b
  match xs
    Cons(x,xx) -> foldl(xx,f(z,x),f)
    Nil        -> z

pub fun foldl1(xs : list<a>, f : (a,a) -> <exn|e> a) : <exn|e> a
  match xs
    Cons(x,xx) -> xx.foldl(x,f)
    Nil        -> throw("unexpected Nil in std/core/foldl1")

pub fun foldr1(xs : list<a>, f : (a,a) -> <exn|e> a) : <exn|e> a
  xs.reverse.foldl1(f)

// Create a list of `n` repeated elements `x`
pub fun replicate( x : a, n : int ) : list<a>
  if n > 0
   then Cons(x, replicate(x,pretend-decreasing(n.dec)))
   else Nil

// split a list at position `n`
pub fun split( xs : list<a>, n : int ) : (list<a>, list<a>)
  (xs.take(n), xs.drop(n))

pub fun span( xs : list<a>, predicate : a -> e bool ) : e (list<a>,list<a>)
  fun span-acc( ys, acc )
    match ys
      Cons(y,yy) -> if y.predicate then yy.span-acc(acc ++ ctx Cons(y, _)) else (acc ++. [],ys)
      _ -> (acc ++. [], ys)
  xs.span-acc( ctx _ )

// Keep only those initial elements that satisfy `predicate`
pub fun take-while( xs : list<a>, predicate : a -> e bool ) : e list<a>
  match xs
    Cons(x,xx) -> if x.predicate then Cons(x, xx.take-while(predicate) ) else Nil
    Nil -> Nil

// Drop all initial elements that satisfy `predicate`
pub fun drop-while( xs : list<a>, predicate : a -> e bool ) : e list<a>
  match xs
    Cons(x,xx) -> if x.predicate then xx.drop-while(predicate) else xs
    Nil -> Nil

// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd?) == [1,3]`
pub fun filter( xs : list<a>, pred : a -> e bool ) : e list<a>
  match xs
    Cons(x,xx) -> if pred(x) then Cons(x,xx.filter(pred)) else xx.filter(pred)
    Nil -> Nil

// Remove those elements of a list that satisfy the given predicate `pred`.
// For example: `remove([1,2,3],odd?) == [2]`
pub fun remove( xs : list<a>, pred : a -> e bool ) : e list<a>
  xs.filter( fn(x) !pred(x) )

// Partition a list in two lists where the first list contains
// those elements that satisfy the given predicate `pred`.
// For example: `partition([1,2,3],odd?) == ([1,3],[2])`
pub fun partition( xs : list<a>, ^pred : a -> e bool ) : e (list<a>,list<a>)
  partition-acc(xs, pred, ctx _, ctx _)

fun partition-acc(xs : list<a>, ^pred : a -> e bool, acc1 : ctx<list<a>>, acc2 : ctx<list<a>>): e (list<a>, list<a>)
  match xs
    Nil -> (acc1 ++. Nil, acc2 ++. Nil)
    Cons(x,xx) -> if pred(x)
      then partition-acc(xx,pred,acc1 ++ ctx Cons(x,_),acc2)
      else partition-acc(xx,pred,acc1,acc2 ++ ctx Cons(x,_))

// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if i.odd? then Nothing else Just(i*i) }) == [4]`
pub fun filter-map( xs : list<a>, pred : a -> e maybe<b> ) : e list<b>
  match xs
    Nil -> Nil
    Cons(x,xx) -> match pred(x)
      Nothing -> xx.filter-map(pred)
      Just(y) -> Cons(y,xx.filter-map(pred))

// Find the first element satisfying some predicate
pub fun find( xs : list<a>, pred : a -> e bool ) : e maybe<a>
  xs.foreach-while fn(x)
    if pred(x) then Just(x) else Nothing

// Find the first element satisfying some predicate and return it.
pub fun find-maybe( xs : list<a>, pred : a -> e maybe<b> ) : e maybe<b>
  xs.foreach-while(pred)

// Lookup the first element satisfying some predicate
pub fun lookup( xs : list<(a,b)>, pred : a -> e bool ) : e maybe<b>
  xs.foreach-while fn(kv)
    if pred(kv.fst) then Just(kv.snd) else Nothing


// Convert a `:maybe` type to a list type.
pub fun maybe/list( m : maybe<a> ) : list<a>
  match m
    Nothing -> Nil
    Just(x) -> Cons(x,Nil)

fun index-of-acc( xs : list<a>, pred : a -> e bool, idx : int ) : e int
  match xs
    Cons(x,xx) -> if pred(x) then idx else index-of-acc(xx,pred,idx+1)
    Nil        -> -1


// Returns the index of the first element where `pred` holds, or `-1` if no such element exists.
pub fun index-of( xs : list<a>, pred : a -> e bool ) : e int
  index-of-acc( xs, pred, 0 )

// Invoke `action` for each element of a list
pub fun foreach( xs : list<a>, action : (a) -> e () ) : e ()
  match xs
    Cons(x,xx) -> { action(x); xx.foreach(action) }
    Nil        -> ()

// Invoke `action` for each element of a list while `action` return `Nothing`
pub fun foreach-while( xs : list<a>, action : (a) -> e maybe<b> ) : e maybe<b>
  match xs
    Nil        -> Nothing
    Cons(x,xx) ->
      match action(x)
        Nothing -> xx.foreach-while(action)
        just    -> just

// Invoke `action` on each element of a list while `action` returns `Just`
pub fun map-while( xs : list<a>, action : (a) -> e maybe<b> ) : e list<b>
  match xs
    Nil        -> Nil
    Cons(x,xx) ->
      match action(x)
        Just(y) -> Cons(y,xx.map-while(action))
        Nothing -> Nil


// Invoke `action` for each element of a list, passing also the position of the element.
pub fun foreach-indexed( xs : list<a>, action : (int,a) -> e () ) : e ()
  var i := 0
  xs.foreach fn(x)
    val j = i // don't dereference `i` inside the inject
    mask<local>{ action(j,x) }
    i := i+1

// Insert a separator `sep`  between all elements of a list `xs` .
pub fun intersperse( xs : list<a>, sep : a ) : list<a>
  fun before(ys)
    match ys
      Cons(y,yy) -> Cons(sep,Cons(y,before(yy)))
      Nil        -> Nil
  match xs
    Cons(x,xx)  -> Cons(x, xx.before)
    Nil         -> Nil

// Concatenate all strings in a list
fun joinsep( xs : list<string>, sep : string ) : string
  match xs
    Nil                 -> ""
    Cons(x,Nil)         -> x
    Cons(x,Cons(y,Nil)) | sep.is-empty -> x ++ y
    _                   -> xs.vector.join(sep)

// Concatenate all strings in a list
pub fun join( xs : list<string> ) : string
  match xs
    Nil                 -> ""
    Cons(x,Nil)         -> x
    Cons(x,Cons(y,Nil)) -> x ++ y
    _                   -> xs.vector.join()

// Concatenate all strings in a list using a specific separator
pub fun joinsep/join( xs : list<string>, sep : string ) : string
  xs.joinsep(sep)

// Concatenate all strings in a list in reverse order
pub fun reverse-join( xs : list<string> ) : string
  xs.reverse.join

// Append `end` to each string in the list `xs` and join them all together.\
// `join-end([],end) === ""`\
// `join-end(["a","b"],"/") === "a/b/"`
pub fun join-end( xs : list<string>, end : string) : string
  match xs
    Nil -> ""
    _   -> xs.joinsep(end) ++ end

// Concatenate all lists in a list (e.g. flatten the list). (tail-recursive)
pub fun concat( xss : list<list<a>> ) : list<a>
  fun concat-pre( ys : list<a>, zss : list<list<a>> ) : list<a>
    match ys
      Cons(y,yy) -> Cons(y,concat-pre(yy,zss))
      Nil -> match zss
        Cons(zs,zzs) -> concat-pre(zs,zzs)
        Nil -> Nil
  concat-pre([],xss)

// Concatenate the result lists from applying a function to all elements.
pub fun flatmap( xs: list<a>, f : a -> e list<b> ) : e list<b>
  fun flatmap-pre( ys, zs )
    match ys
      Cons(y,yy) -> Cons(y,flatmap-pre(yy,zs))
      Nil -> match zs
        Cons(z,zz) -> flatmap-pre(f(z),zz)
        Nil -> Nil
  flatmap-pre([],xs)

// Concatenate the `Just` result elements from applying a function to all elements.
pub fun flatmap-maybe( xs : list<a>, f : a -> e maybe<b> ) : e list<b>
  match xs
    Cons(x,xx) -> match f(x)
      Just(y) -> Cons(y, xx.flatmap-maybe(f))
      Nothing -> xx.flatmap-maybe(f)
    Nil -> Nil

// Concatenate a list of `:maybe` values
pub fun concat-maybe( xs : list<maybe<a>> ) : list<a>
  match xs
    Cons(x,xx) -> match x
      Just(y) -> Cons(y, xx.concat-maybe)
      Nothing -> xx.concat-maybe
    Nil -> Nil

// Return the last element of a list (or `Nothing` for the empty list)
pub fun last( xs : list<a> ) : maybe<a>
  match xs
    Cons(x,Nil) -> Just(x)
    Cons(_,xx)  -> last(xx)
    Nil         -> Nothing

// Return the list without its last element.
// Return an empty list for an empty list.
pub fun init( xs : list<a> ) : list<a>
  match xs
    Cons(x, xx as Cons) -> Cons(x,init(xx))
    _ -> Nil

// Get (zero-based) element `n`  of a list. Return a `:maybe` type.
pub fun @index( xs : list<a>, n : int ) : maybe<a>
  match xs
    Cons(x,xx) -> if n>0 then xx[n - 1]   // == @index(xx,n-1)
                  elif n==0 then Just(x)
                  else Nothing
    Nil -> Nothing

// Do all elements satisfy a predicate ?
pub fun all( xs : list<a>, predicate : a -> e bool ) : e bool
  match xs
    Cons(x,xx) -> if predicate(x) then xx.all(predicate) else False
    Nil -> True

// Are there any elements in a list that satisfy a predicate ?
pub fun any( xs : list<a>, predicate : a -> e bool ) : e bool
  match xs
    Cons(x,xx) -> if predicate(x) then True else xx.any(predicate)
    Nil -> False

// Return the sum of a list of integers
pub fun sum( xs : list<int> ) : int
  xs.foldl( 0, fn(x,y) { x + y } )

// Returns the smallest element of a list of integers (or `default` (=`0`) for the empty list)
pub fun minimum( xs : list<int>, default : int = 0 ) : int
  match xs
    Nil -> default
    Cons(x,xx) -> xx.foldl( x, min )

// Returns the largest element of a list of integers (or `default` (=`0`) for the empty list)
pub fun maximum( xs : list<int>, default : int = 0 ) : int
  match xs
    Nil -> default
    Cons(x,xx) -> xx.foldl( x, max )


// Split a string into a list of lines
pub fun lines( s : string ) : list<string>
  s.split("\n")

// Join a list of strings with newlines
pub fun unlines( xs : list<string> ) : string
  xs.join("\n")

// Apply a function `f` to each character in a string
pub fun string/map( s : string, f : char -> e char ) : e string
  s.list.map(f).string // todo: optimize
