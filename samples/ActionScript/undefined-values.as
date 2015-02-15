var foo; // untyped
var bar:*; // explicitly untyped

trace(foo + ", " + bar); // outputs "undefined, undefined"

if (foo == undefined)
    trace("foo is undefined"); // outputs "foo is undefined"
