function example(options) {
  // assign some defaults where the user's has not provided a value
  opts = {}
  opts.foo = options.foo || 0;
  opts.bar = options.bar || 1;
  opts.grill = options.grill || 'pork chops'

  alert("foo is " + opts.foo + ", bar is " + opts.bar + ", and grill is " + opts.grill);
}

example({grill: "lamb kebab", bar: 3.14});
// => "foo is 0, bar is 3.14, and grill is lamb kebab"
