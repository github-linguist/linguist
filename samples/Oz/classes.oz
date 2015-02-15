declare
  class Something
     feat
        name %% immutable, public attribute (called a "feature")
     attr
        count %% mutable, private attribute

     %% public method which is used as an initializer
     meth init(N)
        self.name = N
        count := 0
     end

     %% public method
     meth increase
        count := @count + 1
     end
  end
in
  %% create an instance
  Object = {New Something init("object")}

  %% call a method
  {Object increase}
