var example = new Object;
example.foo = function () {
  alert("this is foo");
}
example.bar = function () {
  alert("this is bar");
}
example.__noSuchMethod__ = function (id, args) {
  alert("tried to handle unknown method " + id);
  if (args.length != 0)
    alert("it had arguments: " + args);
}

example.foo();        // alerts "this is foo"
example.bar();        // alerts "this is bar"
example.grill();      // alerts "tried to handle unknown method grill"
example.ding("dong"); // alerts "tried to handle unknown method ding"
                      // alerts "it had arguments: dong
