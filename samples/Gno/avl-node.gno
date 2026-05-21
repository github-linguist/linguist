package avl

//----------------------------------------
// Node

type Node struct {
	key       string
	value     interface{}
	height    int8
	size      int
	leftNode  *Node
	rightNode *Node
}

func NewNode(key string, value interface{}) *Node {
	return &Node{
		key:    key,
		value:  value,
		height: 0,
		size:   1,
	}
}

func (node *Node) Size() int {
	if node == nil {
		return 0
	}
	return node.size
}

func (node *Node) IsLeaf() bool {
	return node.height == 0
}

func (node *Node) Key() string {
	return node.key
}

func (node *Node) Value() interface{} {
	return node.value
}

func (node *Node) _copy() *Node {
	if node.height == 0 {
		panic("Why are you copying a value node?")
	}
	return &Node{
		key:       node.key,
		height:    node.height,
		size:      node.size,
		leftNode:  node.leftNode,
		rightNode: node.rightNode,
	}
}

func (node *Node) Has(key string) (has bool) {
	if node == nil {
		return false
	}
	if node.key == key {
		return true
	}
	if node.height == 0 {
		return false
	} else {
		if key < node.key {
			return node.getLeftNode().Has(key)
		} else {
			return node.getRightNode().Has(key)
		}
	}
}

func (node *Node) Get(key string) (index int, value interface{}, exists bool) {
	if node == nil {
		return 0, nil, false
	}
	if node.height == 0 {
		if node.key == key {
			return 0, node.value, true
		} else if node.key < key {
			return 1, nil, false
		} else {
			return 0, nil, false
		}
	} else {
		if key < node.key {
			return node.getLeftNode().Get(key)
		} else {
			rightNode := node.getRightNode()
			index, value, exists = rightNode.Get(key)
			index += node.size - rightNode.size
			return index, value, exists
		}
	}
}

func (node *Node) GetByIndex(index int) (key string, value interface{}) {
	if node.height == 0 {
		if index == 0 {
			return node.key, node.value
		} else {
			panic("GetByIndex asked for invalid index")
			return "", nil
		}
	} else {
		// TODO: could improve this by storing the sizes
		leftNode := node.getLeftNode()
		if index < leftNode.size {
			return leftNode.GetByIndex(index)
		} else {
			return node.getRightNode().GetByIndex(index - leftNode.size)
		}
	}
}

// XXX consider a better way to do this... perhaps split Node from Node.
func (node *Node) Set(key string, value interface{}) (newSelf *Node, updated bool) {
	if node == nil {
		return NewNode(key, value), false
	}
	if node.height == 0 {
		if key < node.key {
			return &Node{
				key:       node.key,
				height:    1,
				size:      2,
				leftNode:  NewNode(key, value),
				rightNode: node,
			}, false
		} else if key == node.key {
			return NewNode(key, value), true
		} else {
			return &Node{
				key:       key,
				height:    1,
				size:      2,
				leftNode:  node,
				rightNode: NewNode(key, value),
			}, false
		}
	} else {
		node = node._copy()
		if key < node.key {
			node.leftNode, updated = node.getLeftNode().Set(key, value)
		} else {
			node.rightNode, updated = node.getRightNode().Set(key, value)
		}
		if updated {
			return node, updated
		} else {
			node.calcHeightAndSize()
			return node.balance(), updated
		}
	}
}

// newNode: The new node to replace node after remove.
// newKey: new leftmost leaf key for node after successfully removing 'key' if changed.
// value: removed value.
func (node *Node) Remove(key string) (
	newNode *Node, newKey string, value interface{}, removed bool,
) {
	if node == nil {
		return nil, "", nil, false
	}
	if node.height == 0 {
		if key == node.key {
			return nil, "", node.value, true
		} else {
			return node, "", nil, false
		}
	} else {
		if key < node.key {
			var newLeftNode *Node
			newLeftNode, newKey, value, removed = node.getLeftNode().Remove(key)
			if !removed {
				return node, "", value, false
			} else if newLeftNode == nil { // left node held value, was removed
				return node.rightNode, node.key, value, true
			}
			node = node._copy()
			node.leftNode = newLeftNode
			node.calcHeightAndSize()
			node = node.balance()
			return node, newKey, value, true
		} else {
			var newRightNode *Node
			newRightNode, newKey, value, removed = node.getRightNode().Remove(key)
			if !removed {
				return node, "", value, false
			} else if newRightNode == nil { // right node held value, was removed
				return node.leftNode, "", value, true
			}
			node = node._copy()
			node.rightNode = newRightNode
			if newKey != "" {
				node.key = newKey
			}
			node.calcHeightAndSize()
			node = node.balance()
			return node, "", value, true
		}
	}
}

func (node *Node) getLeftNode() *Node {
	return node.leftNode
}

func (node *Node) getRightNode() *Node {
	return node.rightNode
}

// NOTE: overwrites node
// TODO: optimize balance & rotate
func (node *Node) rotateRight() *Node {
	node = node._copy()
	l := node.getLeftNode()
	_l := l._copy()

	_lrCached := _l.rightNode
	_l.rightNode = node
	node.leftNode = _lrCached

	node.calcHeightAndSize()
	_l.calcHeightAndSize()

	return _l
}

// NOTE: overwrites node
// TODO: optimize balance & rotate
func (node *Node) rotateLeft() *Node {
	node = node._copy()
	r := node.getRightNode()
	_r := r._copy()

	_rlCached := _r.leftNode
	_r.leftNode = node
	node.rightNode = _rlCached

	node.calcHeightAndSize()
	_r.calcHeightAndSize()

	return _r
}

// NOTE: mutates height and size
func (node *Node) calcHeightAndSize() {
	node.height = maxInt8(node.getLeftNode().height, node.getRightNode().height) + 1
	node.size = node.getLeftNode().size + node.getRightNode().size
}

func (node *Node) calcBalance() int {
	return int(node.getLeftNode().height) - int(node.getRightNode().height)
}

// NOTE: assumes that node can be modified
// TODO: optimize balance & rotate
func (node *Node) balance() (newSelf *Node) {
	balance := node.calcBalance()
	if balance > 1 {
		if node.getLeftNode().calcBalance() >= 0 {
			// Left Left Case
			return node.rotateRight()
		} else {
			// Left Right Case
			// node = node._copy()
			left := node.getLeftNode()
			node.leftNode = left.rotateLeft()
			// node.calcHeightAndSize()
			return node.rotateRight()
		}
	}
	if balance < -1 {
		if node.getRightNode().calcBalance() <= 0 {
			// Right Right Case
			return node.rotateLeft()
		} else {
			// Right Left Case
			// node = node._copy()
			right := node.getRightNode()
			node.rightNode = right.rotateRight()
			// node.calcHeightAndSize()
			return node.rotateLeft()
		}
	}
	// Nothing changed
	return node
}

// Shortcut for TraverseInRange.
func (node *Node) Iterate(start, end string, cb func(*Node) bool) bool {
	return node.TraverseInRange(start, end, true, true, cb)
}

// Shortcut for TraverseInRange.
func (node *Node) ReverseIterate(start, end string, cb func(*Node) bool) bool {
	return node.TraverseInRange(start, end, false, true, cb)
}

// TraverseInRange traverses all nodes, including inner nodes.
// Start is inclusive and end is exclusive when ascending,
// Start and end are inclusive when descending.
// Empty start and empty end denote no start and no end.
// If leavesOnly is true, only visit leaf nodes.
// NOTE: To simulate an exclusive reverse traversal,
// just append 0x00 to start.
func (node *Node) TraverseInRange(start, end string, ascending bool, leavesOnly bool, cb func(*Node) bool) bool {
	if node == nil {
		return false
	}
	afterStart := (start == "" || start < node.key)
	startOrAfter := (start == "" || start <= node.key)
	beforeEnd := false
	if ascending {
		beforeEnd = (end == "" || node.key < end)
	} else {
		beforeEnd = (end == "" || node.key <= end)
	}

	// Run callback per inner/leaf node.
	stop := false
	if (!node.IsLeaf() && !leavesOnly) ||
		(node.IsLeaf() && startOrAfter && beforeEnd) {
		stop = cb(node)
		if stop {
			return stop
		}
	}
	if node.IsLeaf() {
		return stop
	}

	if ascending {
		// check lower nodes, then higher
		if afterStart {
			stop = node.getLeftNode().TraverseInRange(start, end, ascending, leavesOnly, cb)
		}
		if stop {
			return stop
		}
		if beforeEnd {
			stop = node.getRightNode().TraverseInRange(start, end, ascending, leavesOnly, cb)
		}
	} else {
		// check the higher nodes first
		if beforeEnd {
			stop = node.getRightNode().TraverseInRange(start, end, ascending, leavesOnly, cb)
		}
		if stop {
			return stop
		}
		if afterStart {
			stop = node.getLeftNode().TraverseInRange(start, end, ascending, leavesOnly, cb)
		}
	}

	return stop
}

// TraverseByOffset traverses all nodes, including inner nodes.
// A limit of math.MaxInt means no limit.
func (node *Node) TraverseByOffset(offset, limit int, descending bool, leavesOnly bool, cb func(*Node) bool) bool {
	if node == nil {
		return false
	}

	// fast paths. these happen only if TraverseByOffset is called directly on a leaf.
	if limit <= 0 || offset >= node.size {
		return false
	}
	if node.IsLeaf() {
		if offset > 0 {
			return false
		}
		return cb(node)
	}

	// go to the actual recursive function.
	return node.traverseByOffset(offset, limit, descending, leavesOnly, cb)
}

func (node *Node) traverseByOffset(offset, limit int, descending bool, leavesOnly bool, cb func(*Node) bool) bool {
	// caller guarantees: offset < node.size; limit > 0.

	if !leavesOnly {
		if cb(node) {
			return true
		}
	}
	first, second := node.getLeftNode(), node.getRightNode()
	if descending {
		first, second = second, first
	}
	if first.IsLeaf() {
		// either run or skip, based on offset
		if offset > 0 {
			offset--
		} else {
			cb(first)
			limit--
			if limit <= 0 {
				return false
			}
		}
	} else {
		// possible cases:
		// 1 the offset given skips the first node entirely
		// 2 the offset skips none or part of the first node, but the limit requires some of the second node.
		// 3 the offset skips none or part of the first node, and the limit stops our search on the first node.
		if offset >= first.size {
			offset -= first.size // 1
		} else {
			if first.traverseByOffset(offset, limit, descending, leavesOnly, cb) {
				return true
			}
			// number of leaves which could actually be called from inside
			delta := first.size - offset
			offset = 0
			if delta >= limit {
				return true // 3
			}
			limit -= delta // 2
		}
	}

	// because of the caller guarantees and the way we handle the first node,
	// at this point we know that limit > 0 and there must be some values in
	// this second node that we include.

	// => if the second node is a leaf, it has to be included.
	if second.IsLeaf() {
		return cb(second)
	}
	// => if it is not a leaf, it will still be enough to recursively call this
	// function with the updated offset and limit
	return second.traverseByOffset(offset, limit, descending, leavesOnly, cb)
}

// Only used in testing...
func (node *Node) lmd() *Node {
	if node.height == 0 {
		return node
	}
	return node.getLeftNode().lmd()
}

// Only used in testing...
func (node *Node) rmd() *Node {
	if node.height == 0 {
		return node
	}
	return node.getRightNode().rmd()
}

func maxInt8(a, b int8) int8 {
	if a > b {
		return a
	}
	return b
}
