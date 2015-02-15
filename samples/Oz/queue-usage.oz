declare
  [Queue] = {Link ['x-oz://system/adt/Queue.ozf']}
  MyQueue = {Queue.new}
in
  {MyQueue.isEmpty} = true
  {MyQueue.put foo}
  {MyQueue.put bar}
  {MyQueue.put baz}
  {MyQueue.isEmpty} = false
  {Show {MyQueue.get}}  %% foo
  {Show {MyQueue.get}}  %% bar
  {Show {MyQueue.get}}  %% baz
