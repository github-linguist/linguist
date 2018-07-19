TUPLE: my-class foo bar baz ;
M: my-class quux foo>> 20 + ;
C: <my-class> my-class
10 20 30 <my-class> quux ! result: 30
TUPLE: my-child-class < my-class quxx ;
C: <my-child-class> my-child-class
M: my-child-class foobar 20 >>quux ;
20 20 30 <my-child-class> foobar quux ! result: 30
