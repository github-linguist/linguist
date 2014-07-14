# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Implementation of circular lists
# This example shows the usage of generics and somewhat a specialisation of collections.
module circular_list

# Sequences of elements implemented with a double-linked circular list
class CircularList[E]
	# Like standard Array or LinkedList, CircularList is a Sequence.
	super Sequence[E]

	# The first node of the list if any
	# The special case of an empty list is handled by a null node
	private var node: nullable CLNode[E] = null

	redef fun iterator do return new CircularListIterator[E](self)

	redef fun first do return self.node.item

	redef fun push(e)
	do
		var new_node = new CLNode[E](e)
		var n = self.node
		if n == null then
			# the first node
			self.node = new_node
		else
			# not the first one, so attach nodes correctly.
			var old_last_node = n.prev
			new_node.next = n
			new_node.prev = old_last_node
			old_last_node.next = new_node
			n.prev = new_node
		end
	end

	redef fun pop
	do
		var n = self.node
		assert n != null
		var prev = n.prev
		if prev == n then
			# the only node
			self.node = null
			return n.item
		end
		# not the only one do detach nodes correctly.
		var prev_prev = prev.prev
		n.prev = prev_prev
		prev_prev.next = n
		return prev.item
	end

	redef fun unshift(e)
	do
		# Circularity has benefits.
		push(e)
		self.node = self.node.prev
	end

	redef fun shift
	do
		# Circularity has benefits.
		self.node = self.node.next
		return self.pop
	end

	# Move the first at the last position, the second at the first, etc.
	fun rotate
	do
		var n = self.node
		if n == null then return
		self.node = n.next
	end

	# Sort the list using the Josephus algorithm.
	fun josephus(step: Int)
	do
		var res = new CircularList[E]
		while not self.is_empty do
			# count 'step'
			for i in [1..step[ do self.rotate
			# kill
			var x = self.shift
			res.add(x)
		end
		self.node = res.node
	end
end

# Nodes of a CircularList
private class CLNode[E]
	# The current item
	var item: E

	# The next item in the circular list.
	# Because of circularity, there is always a next;
	# so by default let it be self
	var next: CLNode[E] = self

	# The previous item in the circular list.
	# Coherence between next and previous nodes has to be maintained by the
	# circular list.
	var prev: CLNode[E] = self
end

# An iterator of a CircularList.
private class CircularListIterator[E]
	super IndexedIterator[E]

	redef var index: Int

	# The current node pointed.
	# Is null if the list is empty.
	var node: nullable CLNode[E]

	# The list iterated.
	var list: CircularList[E]

	redef fun is_ok
	do
		# Empty lists are not OK.
		# Pointing again the first node is not OK.
		return self.node != null and (self.index == 0 or self.node != self.list.node)
	end

	redef fun next
	do
		self.node = self.node.next
		self.index += 1
	end

	redef fun item do return self.node.item

	init(list: CircularList[E])
	do
		self.node = list.node
		self.list = list
		self.index = 0
	end
end

var i = new CircularList[Int]
i.add_all([1, 2, 3, 4, 5, 6, 7])
print i.first
print i.join(":")

i.push(8)
print i.shift
print i.pop
i.unshift(0)
print i.join(":")

i.josephus(3)
print i.join(":")
