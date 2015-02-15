example = new Object;
example.foo = function(x) {
    return 42 + x;
};

name = "foo";
example[name](5)      # => 47
