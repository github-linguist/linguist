class CatchThemAll {
  def foo {
    "foo received" println
  }

  def bar {
    "bar received" println
  }

  def unknown_message: msg with_params: params {
    "message: " ++ msg print
    "arguments: " ++ (params join: ", ") println
  }
}

a = CatchThemAll new
a foo
a bar
a we_can_do_it
a they_can_too: "eat" and: "walk"
