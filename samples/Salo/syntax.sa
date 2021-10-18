-- This is a comment.

|| This is a doc comment.
myvalue : String
myvalue = "This is an immutable value, myvalue, with the type of String."

myfunction : String -> String
myfunction "" = "This is a function that pattern matches on its first argument."
myfunction _  = "If the first argument is a blank String, it evaluates to ^"
-- Else, `myfunction` evaluates to ^.

-- This is a type declaration. Here, we declare that the type `MyType` as EITHER
-- `True` or `False`.
type MyType = True | False

-- This is a dependent type declaration. Here, we declare `MyDependentType` to
-- be a type that depends on two Nats.
type MyDependentType = Nat -> Nat

{--
  Here's a block comment.

  We can have quite long block comments!
--}

{||
  Here's a block doc comment.

  We can have quite long block doc comments, too!
||}

-- Functions can also be polymorphic
f : a -> a
f x = x

-- `f` can now be called on anything.

-- In this sample file, we didn't cover imports.
