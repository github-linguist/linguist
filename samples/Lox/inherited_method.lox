class Foo {
  inFoo() {
    print "in foo";
  }
}

class Bar < Foo {
  inBar() {
    print "in bar";
  }
}

class Baz < Bar {
  inBaz() {
    print "in baz";
  }
}

var baz = Baz();
baz.inFoo(); // expect: in foo
baz.inBar(); // expect: in bar
baz.inBaz(); // expect: in baz
