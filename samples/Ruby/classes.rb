class MyClass
  @@class_var = []

  def initialize
    # 'initialize' is the constructor method invoked during 'MyClass.new'
    @instance_var = 0
  end

  def some_method
    @instance_var = 1
    @@class_var << Time.now
  end

  def self.class_method
    # ...
  end

  # another way to define class methods: define an instance method in this class object's singleton class
  class << self
    def another_class_method
      # ...
    end
  end
end

myclass = MyClass.new
