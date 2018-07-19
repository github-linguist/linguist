require 'singleton'
class MySingleton
   include Singleton
   # constructor and/or methods go here
end

a = MySingleton.instance # instance is only created the first time it is requested
b = MySingleton.instance
puts a.equal?(b) # outputs "true"
