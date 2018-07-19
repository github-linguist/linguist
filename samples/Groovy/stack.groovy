def stack = []
assert stack.empty

stack.push(55)
stack.push(21)
stack.push('kittens')
assert stack.last() == 'kittens'
assert stack.size() == 3
assert ! stack.empty

println stack

assert stack.pop() == "kittens"
assert stack.size() == 2

println stack

stack.push(-20)

println stack

stack.push( stack.pop() * stack.pop() )
assert stack.last() == -420
assert stack.size() == 2

println stack

stack.push(stack.pop() / stack.pop())
assert stack.size() == 1

println stack

println stack.pop()
assert stack.size() == 0
assert stack.empty

try { stack.pop() } catch (NoSuchElementException e) { println e.message }
