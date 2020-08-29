@ocaml.doc(" Immutable arrays are covariant. ")
type t<+'a>

@ocaml.doc(" Redefine the [_] syntax, and disable the assignment [_] = _. ")
module Array: {
  let get: (t<'a>, int) => option<'a>
}

@ocaml.doc(" Converting from/to normal arrays involves making a copy. ")
let fromArray: array<'a> => t<'a>

let toArray: t<'a> => array<'a>

@ocaml.doc(" Subset of the Belt.Array oprerations that do not mutate the array. ")
let length: t<'a> => int

let size: t<'a> => int

let get: (t<'a>, int) => option<'a>

let getExn: (t<'a>, int) => 'a

let getUnsafe: (t<'a>, int) => 'a

let getUndefined: (t<'a>, int) => Js.undefined<'a>

let shuffle: t<'a> => t<'a>

let reverse: t<'a> => t<'a>

let makeUninitialized: int => t<Js.undefined<'a>>

let makeUninitializedUnsafe: int => t<'a>

let make: (int, 'a) => t<'a>

let range: (int, int) => t<int>

let rangeBy: (int, int, ~step: int) => t<int>

let makeByU: (int, (. int) => 'a) => t<'a>
let makeBy: (int, int => 'a) => t<'a>

let makeByAndShuffleU: (int, (. int) => 'a) => t<'a>
let makeByAndShuffle: (int, int => 'a) => t<'a>

let zip: (t<'a>, t<'b>) => t<('a, 'b)>

let zipByU: (t<'a>, t<'b>, (. 'a, 'b) => 'c) => t<'c>
let zipBy: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>

let unzip: t<('a, 'a)> => (t<'a>, t<'a>)

let concat: (t<'a>, t<'a>) => t<'a>

let concatMany: t<t<'a>> => t<'a>

let slice: (t<'a>, ~offset: int, ~len: int) => t<'a>

let sliceToEnd: (t<'a>, int) => t<'a>

let copy: t<'a> => t<'a>

let forEachU: (t<'a>, (. 'a) => unit) => unit
let forEach: (t<'a>, 'a => unit) => unit

let mapU: (t<'a>, (. 'a) => 'b) => t<'b>
let map: (t<'a>, 'a => 'b) => t<'b>

let keepWithIndexU: (t<'a>, (. 'a, int) => bool) => t<'a>
let keepWithIndex: (t<'a>, ('a, int) => bool) => t<'a>

let keepMapU: (t<'a>, (. 'a) => option<'b>) => t<'b>
let keepMap: (t<'a>, 'a => option<'b>) => t<'b>

let forEachWithIndexU: (t<'a>, (. int, 'a) => unit) => unit
let forEachWithIndex: (t<'a>, (int, 'a) => unit) => unit

let mapWithIndexU: (t<'a>, (. int, 'a) => 'b) => t<'b>
let mapWithIndex: (t<'a>, (int, 'a) => 'b) => t<'b>

let partitionU: (t<'a>, (. 'a) => bool) => (t<'a>, t<'a>)
let partition: (t<'a>, 'a => bool) => (t<'a>, t<'a>)

let reduceU: (t<'a>, 'b, (. 'b, 'a) => 'b) => 'b
let reduce: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

let reduceReverseU: (t<'a>, 'b, (. 'b, 'a) => 'b) => 'b
let reduceReverse: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

let reduceReverse2U: (t<'a>, t<'b>, 'c, (. 'c, 'a, 'b) => 'c) => 'c
let reduceReverse2: (t<'a>, t<'b>, 'c, ('c, 'a, 'b) => 'c) => 'c

let someU: (t<'a>, (. 'a) => bool) => bool
let some: (t<'a>, 'a => bool) => bool

let everyU: (t<'a>, (. 'a) => bool) => bool
let every: (t<'a>, 'a => bool) => bool

let every2U: (t<'a>, t<'b>, (. 'a, 'b) => bool) => bool
let every2: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

let some2U: (t<'a>, t<'b>, (. 'a, 'b) => bool) => bool
let some2: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

let cmpU: (t<'a>, t<'a>, (. 'a, 'a) => int) => int
let cmp: (t<'a>, t<'a>, ('a, 'a) => int) => int

let eqU: (t<'a>, t<'a>, (. 'a, 'a) => bool) => bool
let eq: (t<'a>, t<'a>, ('a, 'a) => bool) => bool