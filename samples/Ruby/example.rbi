# typed: strict

# Declares a module
module MyModule
end

# Declares a class
class Parent
  # Declares what modules this class mixes in
  include MyModule
  extend MyModule

  # Declares an untyped method
  def foo
  end

  # Declares a method with input and output types
  sig {params(x: Integer).returns(String)}
  def bar(x)
  end
end

# Declares a class that subclasses another class
class Child < Parent
  # Declares an Integer constant, with an arbitrary value
  X = T.let(T.unsafe(nil), Integer)
end
