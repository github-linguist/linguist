/*
Credit to Wikipedia's article on [Red-black
tree](http://en.wikipedia.org/wiki/Red–black_tree)

**Note:** doesn't handle duplicate entries. This is by design.

## Overview example:

```
var rbt = new RedBlackTree([7, 5, 1, 8])
rbt.add(2) // => 2
rbt.add(10) // => 10
rbt.has(5) // => true
rbt.remove(8) // => 8
```

## Properties:

- size: The total number of items.
*/

type nodeColor =
  | Red
  | Black

/*
Property of a red-black tree, taken from Wikipedia:
1. A node is either red or black.
2. Root is black.
3. Leaves are all null and considered black.
4. Both children of a red node are black.
5. Every path from a node to any of its descendent leaves contains the same
number of black nodes.
*/

type rec node<'value> = {
  mutable left: option<node<'value>>,
  mutable right: option<node<'value>>,
  mutable parent: option<node<'value>>,
  mutable sum: float,
  mutable color : nodeColor,
  mutable height: float,
  mutable value: 'value,
}

type t<'value> = {
  mutable size: int,
  mutable root: option<node<'value>>,
  compare: (. 'value, 'value) => int,
}

let createNode = (~color, ~value, ~height) =>
  {left:None, right:None, parent:None, sum:0., height, value, color}

external castNotOption: option<'a> => 'a = "%identity"

let updateSum = (node) => {
  let leftSum = switch node.left {
  | None => 0.
  | Some(left) => left.sum
  }
  let rightSum = switch node.right {
  | None => 0.
  | Some(right) => right.sum
  }
  node.sum = leftSum +. rightSum +. node.height
}

/* Update the sum for the node and parents recursively. */
let rec updateSumRecursive = (rbt, node) => {
  updateSum(node)
  switch node.parent {
  | None => ()
  | Some(parent) =>
    rbt->updateSumRecursive(parent)
  }
}

let grandParentOf = node => {
  switch node.parent {
  | None => None
  | Some(ref_) => ref_.parent
  }
}

let isLeft = node => {
  switch node.parent {
  | None => false
  | Some(parent) => Some(node) === parent.left
  }
}

let leftOrRightSet = (~node, x, value) => {
  isLeft(node) ? x.left=value : x.right=value
}

let siblingOf = node => {
  if isLeft(node) {
    castNotOption(node.parent).right
  } else {
    castNotOption(node.parent).left
  }
}

let uncleOf = node => {
  switch grandParentOf(node) {
  | None => None
  | Some(grandParentOfNode) =>
    if isLeft(castNotOption(node.parent)) {
      grandParentOfNode.right
    } else {
      grandParentOfNode.left
    }
  }
}

let rec findNode = (rbt, node, value) => {
  switch node {
  | None => None
  | Some(node) =>
    let cmp = rbt.compare(. value, node.value)
    if cmp === 0 {
      Some(node)
    } else if cmp < 0 {
      findNode(rbt, node.left, value)
    } else {
      findNode(rbt, node.right, value)
    }
  }
}

let has = (rbt, value) => findNode(rbt, rbt.root, value) !== None

let rec peekMinNode = node => switch node {
  | None => None
  | Some(node) =>
    node.left === None ? Some(node) : node.left->peekMinNode
}

let rec peekMaxNode = node => switch node {
  | None => None
  | Some(node) =>
    node.right === None ? Some(node) : node.right->peekMaxNode
}

let rotateLeft = (rbt, node) => {
  let parent = node.parent
  let right = node.right
  switch parent {
    | Some(parent) =>
      parent->leftOrRightSet(~node, right)
    | None =>
      rbt.root = right
  }
  node.parent = right
  let right = right->castNotOption // precondition
  let rightLeft = right.left
  node.right = rightLeft
  switch rightLeft {
    | Some(rightLeft) =>
      rightLeft.parent = Some(node)
    | None =>
      ()
  }
  right.parent = parent
  right.left = Some(node)
  updateSum(node)
  updateSum(right)
}

let rotateRight = (rbt, node) => {
  let parent = node.parent
  let left = node.left
  switch parent {
    | Some(parent) =>
      parent->leftOrRightSet(~node, left)
    | None =>
      rbt.root = left
  }
  node.parent = left
  let left = left->castNotOption // precondition
  let leftRight = left.right
  node.left = leftRight
  switch leftRight {
    | Some(leftRight) =>
      leftRight.parent = Some(node)
    | None =>
      ()
  }
  left.parent = parent
  left.right = Some(node)
  updateSum(node)
  updateSum(left)
}

let rec findInsert = (rbt, node, nodeToInsert, value) => {
  switch node {
    | None => None
    | Some(node) => {
      let cmp = rbt.compare(. value, node.value)
      if cmp === 0 {
        Some(node)
      } else {
        if cmp < 0 {
          if node.left !== None {
            rbt->findInsert(node.left, nodeToInsert, value)
          } else {
            nodeToInsert.parent = Some(node)
            node.left = Some(nodeToInsert)
            None
          }
        } else {
          if node.right !== None {
            rbt->findInsert(node.right, nodeToInsert, value)
          } else {
            nodeToInsert.parent = Some(node)
            node.right = Some(nodeToInsert)
            None
          }
        }
      }
    }
  }
}

// After adding the node, we need to operate on it to preserve the tree's
// properties by filtering it through a series of cases. It'd be easier if
// there's tail recursion in JavaScript, as some cases fix the node but
// restart the cases on the node's ancestor. We'll have to use loops for now.

let rec _addLoop = (rbt, currentNode) => {
  // Case 1: node is root. Violates 1. Paint it black.
  if Some(currentNode) === rbt.root {
    currentNode.color = Black
  }

  // Case 2: parent black. No properties violated. After that, parent is sure
  // to be red.
  else if (currentNode.parent->castNotOption).color === Black {
    ()
  }

  // Case 3: if node's parent and uncle are red, they are painted black.
  // Their parent (node's grandparent) should be painted red, and the
  // grandparent red. Note that node certainly has a grandparent, since at
  // this point, its parent's red, which can't be the root.

  // After the painting, the grandparent might violate 2 or 4.
  else if({
      let uncle = uncleOf(currentNode)
      uncle !== None && (uncle->castNotOption).color === Red
    }) {
    (currentNode.parent->castNotOption).color = Black
    (uncleOf(currentNode)->castNotOption).color = Black
    (grandParentOf(currentNode)->castNotOption).color = Red
    _addLoop(rbt, grandParentOf(currentNode)->castNotOption)
  }
  else {
    // At this point, uncle is either black or doesn't exist.

    // Case 4: parent red, uncle black, node is right child, parent is left
    // child. Do a left rotation. Then, former parent passes through case 5.
    let currentNode =
      if !isLeft(currentNode) && isLeft(currentNode.parent->castNotOption) {
        rotateLeft(rbt, currentNode.parent->castNotOption)
        currentNode.left->castNotOption
      } else if isLeft(currentNode) && !isLeft(currentNode.parent->castNotOption) {
        rotateRight(rbt, currentNode.parent->castNotOption)
        currentNode.right->castNotOption
      } else {
        currentNode
      }

    // Case 5: parent red, uncle black, node is left child, parent is left
    // child. Right rotation. Switch parent and grandparent's color.
    (currentNode.parent->castNotOption).color = Black
    (grandParentOf(currentNode)->castNotOption).color = Red
    if isLeft(currentNode) {
      rotateRight(rbt, grandParentOf(currentNode)->castNotOption)
    } else {
      rotateLeft(rbt, grandParentOf(currentNode)->castNotOption)
    }
  }
}

let add = (rbt, value, ~height) => {
  // Again, make sure to not pass a value already in the tree.
  //
  // _Returns:_ value added.
  rbt.size = rbt.size + 1
  let nodeToInsert = createNode(~value, ~color=Red, ~height)
  let inserted =
    if rbt.root === None {
      rbt.root = Some(nodeToInsert)
      true
    }
    else {
      let foundNode = findInsert(rbt, rbt.root, nodeToInsert, value)
      foundNode === None
    }
  if inserted {
    rbt->updateSumRecursive(nodeToInsert)

    _addLoop(rbt, nodeToInsert)
    Some(nodeToInsert)
  } else {
    None
  }
}


// To simplify removal cases, we can notice this:
// 1. Node has no child.
// 2. Node has two children. Select the smallest child on the right branch
// (leftmost) and copy its value into the node to delete. This replacement node
// certainly has less than two children or it wouldn't be the smallest. Then
// delete this replacement node.
// 3. Node has one child.
// They all come down to removing a node with maximum one child.
let removeNode = (rbt, node) => {
  let nodeToRemove =
    switch (node.left, node.right) {
    | (Some(_), Some(_)) =>
      let successor = peekMinNode(node.right)->castNotOption
      node.value = successor.value
      node.height = successor.height
      successor
    | _ => node
    }
  // At this point, the node to remove has only one child.
  let successor = switch nodeToRemove.left {
  | None => nodeToRemove.right
  | left => left
  }
  let (successor, isLeaf) = switch successor {
    | None =>
      let leaf = createNode(~value=%bs.raw("0"), ~color=Black, ~height=0.)
      let isLeaf = (. x) => x === leaf;
      (leaf, isLeaf)
    | Some(successor) =>
      (successor, (. _) => false)
  }
  let nodeParent = nodeToRemove.parent
  successor.parent = nodeParent
  switch nodeParent {
  | None => ()
  | Some(parent) =>
    parent->leftOrRightSet(~node=nodeToRemove, Some(successor))
  }

  rbt->updateSumRecursive(successor)

  // We're done if node's red. If it's black and its child that took its place
  // is red, change it to black. If both are black, we do cases checking like
  // in insert.
  if nodeToRemove.color === Black {
    if successor.color === Red {
      successor.color = Black
      if successor.parent === None {
        rbt.root = Some(successor)
      }
    } else {
      let break = ref(false)
      let successorRef = ref(successor)
      while !break.contents {
        let successor = successorRef.contents
        // Case 1: node is root. Done.
        switch successor.parent {
        | None =>
          rbt.root = Some(successor)
          break.contents = true
        | Some(successorParent) =>
          // Case 2: sibling red. Flip color of P and S. Left rotate P.
          let sibling = siblingOf(successor)
          if sibling !== None && (sibling->castNotOption).color === Red {
            successorParent.color = Red
            (sibling->castNotOption).color = Black
            if isLeft(successor) {
              rotateLeft(rbt, successorParent)
            } else {
              rotateRight(rbt, successorParent)
            }
          }

          // Case 3: parent, sibling and sibling children all black. Paint
          // sibling red. Rebalance parent.
          let sibling = siblingOf(successor)
          let siblingNN = sibling->castNotOption
          if
            successorParent.color === Black &&
            ( sibling === None ||
              ( siblingNN.color === Black &&
                ( siblingNN.left === None ||
                  (siblingNN.left->castNotOption).color === Black ) &&
                ( siblingNN.right === None ||
                  (siblingNN.right->castNotOption).color === Black)))
             {
            if sibling !== None {
              siblingNN.color = Red
            }
            successorRef.contents = successorParent
            // continue
          } else if
            // Case 4: sibling and sibling children black. Node parent red. Swap
            // color of sibling and node parent.
            successorParent.color === Red &&
            ( sibling === None ||
              ( siblingNN.color === Black &&
              ( siblingNN.left === None ||
                (siblingNN.left->castNotOption).color === Black) &&
              ( siblingNN.right === None ||
                (siblingNN.right->castNotOption).color === Black)))
             {
            if sibling !== None {
              siblingNN.color = Red
            }
            successorParent.color = Black
            break.contents = true
          } else if
            // Case 5: sibling black, sibling left child red, right child black,
            // node is left child. Rotate right sibling. Swap color of sibling and
            // its new parent.
            sibling !== None && (sibling->castNotOption).color === Black
             {
            let sibling = sibling->castNotOption
            if
              isLeft(successor) &&
              (sibling.right === None || (sibling.right->castNotOption).color === Black) &&
              sibling.left !== None &&
              (sibling.left->castNotOption).color === Red {
              sibling.color = Red
              (sibling.left->castNotOption).color = Black
              rotateRight(rbt, sibling)
            } else if
              !isLeft(successor) &&
              (sibling.left === None || (sibling.left->castNotOption).color === Black) &&
              sibling.right !== None &&
              (sibling.right->castNotOption).color === Red
               {
              sibling.color = Red
              (sibling.right->castNotOption).color = Black
              rotateLeft(rbt, sibling)
            }
            break.contents = true
          } else {
            // Case 6: sibling black, sibling right child red, node is left child.
            // Rotate left node parent. Swap color of parent and sibling. Paint
            // sibling right child black.
            let sibling = siblingOf(successor)
            let sibling = sibling->castNotOption
            sibling.color = successorParent.color
            if isLeft(successor) {
              (sibling.right->castNotOption).color = Black
              rotateRight(rbt, successorParent)
            } else {
              (sibling.left->castNotOption).color = Black
              rotateLeft(rbt, successorParent)
            }
          }
        }
      }
    }
  }
  // Don't forget to detatch the artificially created leaf.
  if isLeaf(. successor) {
    if rbt.root === Some(successor) {
      rbt.root = None
    }
    switch successor.parent {
    | None => ()
    | Some(parent) =>
      parent->leftOrRightSet(~node=successor, None)
    }
  }
}

let remove = (rbt, value) => {
  switch findNode(rbt, rbt.root, value) {
    | Some(node) =>
      rbt->removeNode(node)
      rbt.size = rbt.size - 1
      true
    | None =>
      false
  }
}

let rec findNodeThroughCallback = (rbt, node, cb) => {
  switch node {
  | None => None
  | Some(node) =>
    let cmp = cb(. node)
    if cmp === 0 {
      Some(node)
    } else if cmp < 0 {
      findNodeThroughCallback(rbt, node.left, cb)
    } else {
      findNodeThroughCallback(rbt, node.right, cb)
    }
  }
}

let removeThroughCallback = (rbt, cb) => {
  switch findNodeThroughCallback(rbt, rbt.root, cb) {
    | Some(node) =>
      rbt->removeNode(node)
      rbt.size = rbt.size - 1
      true
    | None =>
      false
  }
}

let make = (~compare) => {size: 0, root: None, compare}

let makeWith = (array, ~compare) => {
  let rbt = make(~compare)
  array->Js.Array2.forEach(((value, height)) => add(rbt,value, ~height)->ignore)
  rbt
}

// sum of the heights of the elements in [lhs ... rhs]
// both lhs and rhs are optional
let rec heightOfInterval = (rbt, node, lhs, rhs) => {
  switch node {
  | None => 0.
  | Some(n) =>
    //Js.log4("heightOfInterval n:", n.value, lhs, rhs)
    if lhs === None && rhs === None {
      n.sum
    } else if lhs !== None && rbt.compare(. n.value, lhs->castNotOption) < 0 {
      // to the lhs of the interval
      rbt->heightOfInterval(n.right, lhs, rhs)
    } else if rhs !== None && rbt.compare(. n.value, rhs->castNotOption) > 0 {
      // to the rhs of the interval
      rbt->heightOfInterval(n.left, lhs, rhs)
    } else {
      // in the interval
      n.height +.
      rbt->heightOfInterval(n.left, lhs, None) +.
      rbt->heightOfInterval(n.right, None, rhs)
    }
  }
}

let heightOfInterval = (rbt, lhs, rhs) => {
  //Js.log("-----------")
  heightOfInterval(rbt, rbt.root, lhs, rhs)
}

// Return a node at y such that y <= top < y + node.height
let rec firstVisibleNode = (node, top) => {
  switch node {
  | None => None
  | Some(node) =>
    //Js.log4("firstVisibleNode", node.value, "top:", top)
    if node.sum <= top {
    // no node is visible
    None
    } else {
      let nodeHeight = node.height
      let sumLeft = switch node.left {
        | None => 0.0
        | Some(left) => left.sum
      }
      if sumLeft > top {
        firstVisibleNode(node.left, top)
      } else if sumLeft +. nodeHeight > top {
        // found
        Some(node)
      } else {
        let offset = sumLeft +. nodeHeight
        firstVisibleNode(node.right, top -. offset)
      }
    }
  }
}

let lastVisibleNode = (node, top) => {
  switch firstVisibleNode(node, top) {
  | None =>
    node->peekMaxNode
  | first => first
  }
}

// Find the value of the first visible node starting from top
let firstVisibleValue = (rbt, ~top) =>
  switch firstVisibleNode(rbt.root, top) {
  | None => None
  | Some(node) => Some(node.value)
}

let rec leftmost = node => switch node.left {
  | None => node
  | Some(node) => node->leftmost
}

let rec firstRightParent = node => {
  switch node.parent {
    | None => None
    | Some(parent) =>
      isLeft(node) ? Some(parent) : parent->firstRightParent
  }
}

let nextNode = node => {
  switch node.right {
  | None =>
    node->firstRightParent
  | Some(right) =>
    Some(right->leftmost)
  }
}

let rec sumLeftSpine = (node, ~fromRightChild) => {
  let leftSpine = switch node.left {
    | None => node.height
    | Some(left) => fromRightChild ? node.height +. left.sum : 0.0
  }
  switch node.parent {
  | None =>
    leftSpine
  | Some(parent) =>
    leftSpine +. parent->sumLeftSpine(~fromRightChild = parent.right === Some(node))
  }
}

let getY = node =>
  node->sumLeftSpine(~fromRightChild=true) -. node.height

let rec iterate = (~inclusive, firstNode, lastNode, ~callback) => {
  switch firstNode {
    | None => ()
    | Some(node) =>
      if inclusive { callback(. node) }
      if firstNode !== lastNode {
        if !inclusive { callback (.node) }
        iterate(~inclusive, node->nextNode, lastNode, ~callback)
      }
  }
}

let rec iterateWithY = (~y=?, ~inclusive, firstNode, lastNode, ~callback) => {
  switch firstNode {
    | None => ()
    | Some(node) =>
      let y = switch y {
        | None => node->getY
        | Some(y) => y
      }
      if inclusive {
        callback(. node, y)
      }
      if firstNode !== lastNode {
        if !inclusive {
          callback (.node, y)
        }
        iterateWithY(~y=y+.node.height, ~inclusive, node->nextNode, lastNode, ~callback)
      }
  }
}

let rec updateSum = (node, ~delta) => switch node {
  | None => ()
  | Some(node) =>
    node.sum = node.sum +. delta
    node.parent->updateSum(~delta)
}

let updateHeight = (node, ~height) => {
  let delta = height -. node.height
  node.height = height
  Some(node)->updateSum(~delta)
}

type oldNewVisible<'value> = {
  mutable old: array<'value>,
  mutable new: array<'value>,
};

let getAnchorDelta = (rbt, ~anchor) => {
  switch anchor {
    | None => 0.0
    | Some((value, y)) =>
      switch rbt->findNode(rbt.root, value) {
        | Some(node) => y -. node->getY
        | None => 0.0
      }
  }
}

let onChangedVisible =
    (
      ~anchor=None,
      rbt,
      ~oldNewVisible,
      ~top as top_,
      ~bottom as bottom_,
      ~appear,
      ~remained,
      ~disappear,
    ) =>
 {
  let old = oldNewVisible.new
  let new = oldNewVisible.old
  // empty new
  new->Js.Array2.removeCountInPlace(~pos=0, ~count=new->Js.Array2.length)->ignore
  oldNewVisible.old = old
  oldNewVisible.new = new

  let anchorDelta = rbt->getAnchorDelta(~anchor)
  //Js.log2("anchorDelta", anchorDelta)
  let top = top_ -. anchorDelta
  let top = top < 0.0 ? 0.0 : top // anchoring can make top negative
  let bottom = bottom_ -. anchorDelta

  let first = firstVisibleNode(rbt.root, top)
  let last = lastVisibleNode(rbt.root, bottom)

  let oldLen = old->Js.Array2.length
  let oldIter = ref(0)
  iterateWithY(~inclusive=true, first, last, ~callback=(. node, y_) => {
    let y = y_ +. anchorDelta
    if y >= 0.0 { // anchoring can make y negative
      while (
        oldIter.contents < oldLen &&
        rbt.compare(. Js.Array2.unsafe_get(old, oldIter.contents), node.value) < 0
      ) {
        disappear(. Js.Array2.unsafe_get(old, oldIter.contents))
        oldIter.contents = oldIter.contents + 1
      }
      new->Js.Array2.push(node.value)->ignore
      if (oldIter.contents < oldLen) {
        let cmp = rbt.compare(. Js.Array2.unsafe_get(old, oldIter.contents), node.value)
        if cmp == 0 {
          remained(. node, y)
          oldIter.contents = oldIter.contents + 1
        } else {
          appear(. node, y)
        }
      } else {
        appear(. node, y)
      }
    }
  })
  while (oldIter.contents < oldLen) {
    disappear(. Js.Array2.unsafe_get(old, oldIter.contents))
    oldIter.contents = oldIter.contents + 1
  }
};