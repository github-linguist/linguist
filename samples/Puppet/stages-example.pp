class foo {
  notify {
    "foo": ;
  }
}

class bar {
  notify {
    "bar": ;
  }
}


node default {
  stage {
    "one": ;
    "two": ;
  }

  class {
    "foo": stage => "one";
    "bar": stage => "two";
  }

  Stage["one"] -> Stage["two"]
}
