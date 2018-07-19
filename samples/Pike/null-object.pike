> mapping bar;
> bar;
Result: 0
> bar = ([ "foo":0 ]);
> bar->foo;
Result 0;
> zero_type(bar->foo);
Result: 0
> bar->baz;
Result: 0
> zero_type(bar->baz);
Result: 1
