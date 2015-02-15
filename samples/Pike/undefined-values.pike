> zero_type(UNDEFINED);
Result: 1
> mapping bar = ([ "foo":"hello" ]);
> zero_type(bar->foo);
Result: 0
> zero_type(bar->baz);
Result: 1
> bar->baz=UNDEFINED;
Result: 0
> zero_type(bar->baz);
Result: 0
