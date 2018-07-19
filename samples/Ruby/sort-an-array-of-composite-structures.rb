Person = Struct.new(:name,:value)
list = [Person.new("Joe",3),
        Person.new("Bill",4),
        Person.new("Alice",20),
        Person.new("Harry",3)]
list.sort_by{|x|x.name}
