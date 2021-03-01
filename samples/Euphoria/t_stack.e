include std/stack.e as s
include std/unittest.e

stack sk

--
-- FIFO testing
--

sk = s:new(s:FIFO)

test_true("FIFO new() #1", stack(sk))
test_true("FIFO is_empty() #1", s:is_empty(sk))

s:push(sk, 10)
s:push(sk, 20)
s:push(sk, 30)

test_equal("FIFO top()", 10, s:top(sk))
test_equal("FIFO size()", 3, s:size(sk))
test_equal("FIFO at() #1", 30, s:at(sk, 0))
test_equal("FIFO at() #2", 30, s:at(sk, 3))
test_equal("FIFO at() #3", 20, s:at(sk, -1))
test_equal("FIFO at() #4", 20, s:at(sk, 2))
test_equal("FIFO at() #5", 10, s:at(sk, -2))
test_equal("FIFO at() #6", 10, s:at(sk, 1))

s:swap(sk)
test_equal("FIFO swap() #1", 20, s:peek_top(sk, 1))
test_equal("FIFO swap() #2", 10, s:peek_top(sk, 2))

s:swap(sk)
test_equal("FIFO swap() #3", 10, s:peek_top(sk, 1))
test_equal("FIFO swap() #4", 20, s:peek_top(sk, 2))

s:dup(sk)
test_equal("FIFO dup() #1", 10, s:peek_top(sk, 1))
test_equal("FIFO dup() #2", 10, s:peek_top(sk, 2))

test_false("FIFO is_empty() #1", s:is_empty(sk))

s:set(sk, 90, 1)
s:set(sk, "", 2)

test_equal("FIFO pop() #1", 90, s:pop(sk))
test_equal("FIFO pop() #2", 3, s:size(sk))
test_equal("FIFO pop() #3", "", s:pop(sk))
test_equal("FIFO pop() #4", 2, s:size(sk))
test_equal("FIFO pop() #5", 20, s:pop(sk))
test_equal("FIFO pop() #6", 1, s:size(sk))
test_equal("FIFO pop() #7", 30, s:pop(sk))
test_equal("FIFO pop() #8", 0, s:size(sk))

s:clear(sk)
test_equal("FIFO clear()", 0, s:size(sk))
test_true("is_empty() #2", s:is_empty(sk))

delete(sk)
test_pass("FIFO delete")

--
-- FILO testing
--

sk = s:new(s:FILO)
test_true("FILO new()", stack(sk))
test_true("FILO is_empty() #1", s:is_empty(sk))

s:push(sk, 10)
s:push(sk, 20)
s:push(sk, 30)

test_false("FILO is_empty() #2", s:is_empty(sk))
test_equal("FILO size() #1", 3, s:size(sk))
test_equal("FILO at() #1", 30, s:peek_top(sk, 1))
test_equal("FILO at() #2", 20, s:peek_top(sk, 2))
test_equal("FILO at() #3", 10, s:peek_top(sk, 3))
test_equal("FILO top() #1", 30, s:top(sk))

test_equal("FILO pop() #1", 30, s:pop(sk))
test_equal("FILO size() after pop() #1", 2, s:size(sk))
test_equal("FILO top() after pop() #1", 20, s:top(sk))

test_equal("FILO pop() #2", 20, s:pop(sk))
test_equal("FILO size() after pop() #2", 1, s:size(sk))
test_equal("FILO top() after pop() #2", 10, s:top(sk))

s:clear(sk)
test_equal("FILO clear()", 0, s:size(sk))
test_true("FILO is_empty() #3", s:is_empty(sk))

s:push(sk, {10,20})
test_equal("FILO push sequence #1 size()", 1, s:size(sk))
test_equal("FILO push sequence #1 top()", {10,20}, s:top(sk))

s:push(sk, {30,40})
test_equal("FILO push sequence #2 size()", 2, s:size(sk))
test_equal("FILO push sequence #2 top()", {30,40}, s:top(sk))

s:dup(sk)
test_equal("FILO dup sequence #1 size()", 3, s:size(sk))
test_equal("FILO dup sequence #1 top()", {30,40}, s:peek_top(sk,1))
test_equal("FILO dup sequence #1 at(-1)", {30,40}, s:peek_top(sk, 2))

test_equal("FILO at #1", {30,40}, s:peek_top(sk, 1))
test_equal("FILO at #3", {30,40}, s:peek_top(sk, 2))
test_equal("FILO at #2", {10,20}, s:peek_top(sk, 3))

s:set(sk, 90, 1)
test_equal("FILO set #1", 90, s:peek_top(sk, 1))

s:set(sk, "", 2)
test_equal("FILO set #2", "", s:peek_top(sk, 2))

s:set( sk, "from the end", -1 )
test_equal("FILO set #3 -1 from the end", "from the end", s:peek_end(sk, 2 ) )

s:set( sk, "last", 0 )
test_equal("FILE set #4 0 frmo the end", "last", s:last(sk) )

delete(sk)
test_pass("FILO delete")

test_report()
