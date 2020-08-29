module IntSum: {
  let x: int 
  let y: int 

  let add: (int, int) => int
}

module LinkedList: module type of List

module Functoria: (X: Int, Y: Float) => Float

module Ext: %extension

module MyInt = Int
module MyInt: Int

module X: A with type t = string

module LongNaaaaame: MyModule
  with module X.Bar = Belt.Array
   and module X.Bar = Belt.Array
   and module X.Bar = Belt.Array


@attr
module LinkedList: module type of List