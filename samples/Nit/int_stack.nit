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

# An example that defines and uses stacks of integers.
# The implementation is done with a simple linked list.
# It features: free constructors, nullable types and some adaptive typing.
module int_stack

# A stack of integer implemented by a simple linked list.
# Note that this is only a toy class since a real linked list will gain to use
# generics and extends interfaces, like Collection, from the standard library.
class IntStack
	# The head node of the list.
	# Null means that the stack is empty.
	private var head: nullable ISNode = null

	# Add a new integer in the stack.
	fun push(val: Int)
	do
		self.head = new ISNode(val, self.head)
	end

	# Remove and return the last pushed integer.
	# Return null if the stack is empty.
	fun pop: nullable Int
	do
		var head = self.head
		if head == null then return null
		# Note: the followings are statically safe because of the
		# previous 'if'.
		var val = head.val
		self.head = head.next
		return val
	end

	# Return the sum of all integers of the stack.
	# Return 0 if the stack is empty.
	fun sumall: Int
	do
		var sum = 0
		var cur = self.head
		while cur != null do
			# Note: the followings are statically safe because of
			# the condition of the 'while'.
			sum += cur.val
			cur = cur.next
		end
		return sum
	end

	# Note: Because all attributes have a default value, a free constructor
	# "init()" is implicitly defined.
end

# A node of a IntStack
private class ISNode
	# The integer value stored in the node.
	var val: Int

	# The next node, if any.
	var next: nullable ISNode

	# Note: A free constructor "init(val: Int, next: nullable ISNode)" is
	# implicitly defined.
end

var l = new IntStack
l.push(1)
l.push(2)
l.push(3)

print l.sumall

# Note: the 'for' control structure cannot be used on IntStack in its current state.
# It requires a more advanced topic.
# However, why not using the 'loop' control structure?
loop
	var i = l.pop
	if i == null then break
	# The following is statically safe because of the previous 'if'.
	print i * 10
end

# Note: 'or else' is used to give an alternative of a null expression.
l.push(5)
print l.pop or else 0 # l.pop gives 5, so print 5
print l.pop or else 0 # l.pop gives null, so print the alternative: 0


