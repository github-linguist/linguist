% Implemented by Arjun Sunel
1> string:strip("   Hello World!   ", left). %remove leading whitespaces
"Hello World!   "

2> string:strip("   Hello World!   ", right). % remove trailing whitespaces
"   Hello World!"

3> string:strip("   Hello World!   ", both).  % remove both leading and trailing whitespace
"Hello World!"
