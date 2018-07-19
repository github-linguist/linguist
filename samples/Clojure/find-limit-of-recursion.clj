=> (def *stack* 0)
=> ((fn overflow [] ((def *stack* (inc *stack*))(overflow))))
java.lang.StackOverflowError (NO_SOURCE_FILE:0)
=> *stack*
10498
