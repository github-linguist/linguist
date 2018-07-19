var foo = eval('{value: 42}');
eval('var bar = "Hello, world!";');

typeof foo; // 'object'
typeof bar; // 'string'
