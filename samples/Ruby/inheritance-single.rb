class Animal
  #functions go here...
  def self.inherited(subclass)
    puts "new subclass of #{self}: #{subclass}"
  end
end

class Dog < Animal
  #functions go here...
end

class Cat < Animal
  #functions go here...
end

class Lab < Dog
  #functions go here...
end

class Collie < Dog
  #functions go here...
end
