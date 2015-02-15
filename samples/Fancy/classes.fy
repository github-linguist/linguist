class MyClass {
  read_slot: 'instance_var # creates getter method for @instance_var
  @@class_var = []

  def initialize {
    # 'initialize' is the constructor method invoked during 'MyClass.new' by convention
    @instance_var = 0
  }

  def some_method {
    @instance_var = 1
    @another_instance_var = "foo"
  }

  # define class methods: define a singleton method on the class object
  def self class_method {
    # ...
  }

  # you can also name the class object itself
  def MyClass class_method {
    # ...
  }
}

myclass = MyClass new
