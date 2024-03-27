type nodeColor = Red | Black

type rec node<'value> = {
  mutable left: option<node<'value>>,
  mutable right: option<node<'value>>,
  mutable parent: option<node<'value>>,
  mutable sum: float,
  mutable color: nodeColor,
  mutable height: float,
  mutable value: 'value,
}

type t<'value> = {
  mutable size: int,
  mutable root: option<node<'value>>,
  compare: (. 'value, 'value) => int,
}

let createNode: (~color: nodeColor, ~value: 'a, ~height: float) => node<'a>

external castNotOption: option<'a> => 'a = "%identity"

let updateSumRecursive: ('a, node<'b>) => unit

let grandParentOf: node<'a> => option<node<'a>>

let isLeft: node<'a> => bool

let leftOrRightSet: (~node: node<'a>, node<'b>, option<node<'b>>) => unit

let siblingOf: node<'a> => option<node<'a>>

let uncleOf: node<'a> => option<node<'a>>

let findNode: (t<'a>, option<node<'a>>, 'a) => option<node<'a>>

let has: (t<'a>, 'a) => bool

let peekMinNode: option<node<'a>> => option<node<'a>>

let peekMaxNode: option<node<'a>> => option<node<'a>>

let rotateLeft: (t<'a>, node<'a>) => unit

let rotateRight: (t<'a>, node<'a>) => unit

let findInsert: (t<'a>, option<node<'a>>, node<'a>, 'a) => option<node<'a>>

let _addLoop: (t<'a>, node<'a>) => unit

let add: (t<'a>, 'a, ~height: float) => option<node<'a>>

let removeNode: (t<'a>, node<'a>) => unit

let remove: (t<'a>, 'a) => bool

let findNodeThroughCallback: ('a, option<node<'b>>, (. node<'b>) => int) => option<node<'b>>

let removeThroughCallback: (t<'a>, (. node<'a>) => int) => bool

let make: (~compare: (. 'a, 'a) => int) => t<'a>

let makeWith: (Js.Array2.t<('a, float)>, ~compare: (. 'a, 'a) => int) => t<'a>

let heightOfInterval: (t<'a>, option<'a>, option<'a>) => float

let firstVisibleNode: (option<node<'a>>, float) => option<node<'a>>

let lastVisibleNode: (option<node<'a>>, float) => option<node<'a>>

let firstVisibleValue: (t<'a>, ~top: float) => option<'a>

let leftmost: node<'a> => node<'a>

let firstRightParent: node<'a> => option<node<'a>>

let nextNode: node<'a> => option<node<'a>>

let sumLeftSpine: (node<'a>, ~fromRightChild: bool) => float

let getY: node<'a> => float

let iterate: (
  ~inclusive: bool,
  option<node<'a>>,
  option<node<'a>>,
  ~callback: (. node<'a>) => unit,
) => unit

let iterateWithY: (
  ~y: float=?,
  ~inclusive: bool,
  option<node<'a>>,
  option<node<'a>>,
  ~callback: (. node<'a>, float) => unit,
) => unit

let updateSum: (option<node<'a>>, ~delta: float) => unit

let updateHeight: (node<'a>, ~height: float) => unit

type oldNewVisible<'value> = {mutable old: array<'value>, mutable new: array<'value>}

let getAnchorDelta: (t<'a>, ~anchor: option<('a, float)>) => float

let onChangedVisible: (
  ~anchor: option<('a, float)>=?,
  t<'a>,
  ~oldNewVisible: oldNewVisible<'a>,
  ~top: float,
  ~bottom: float,
  ~appear: (. node<'a>, float) => unit,
  ~remained: (. node<'a>, float) => unit,
  ~disappear: (. 'a) => unit,
) => unit
