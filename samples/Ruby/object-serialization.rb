class Being
  def initialize(specialty=nil)
    @specialty=specialty
  end
  def to_s
    "(object_id = #{object_id})\n"+"(#{self.class}):".ljust(12)+to_s4Being+(@specialty ? "\n"+" "*12+@specialty : "")
  end
  def to_s4Being
    "I am a collection of cooperative molecules with a talent for self-preservation."
  end
end

class Earthling < Being
  def to_s4Being
    "I originate from a blue planet.\n"+" "*12+to_s4Earthling
  end
end

class Mammal < Earthling
  def initialize(type)
    @type=type
  end
  def to_s4Earthling
    "I am champion in taking care of my offspring and eating everything I can find, except mammals of type #{@type}."
  end
end

class Fish < Earthling
  def initialize(iq)
    @iq=(iq>1 ? :instrustableValue : iq)
  end
  def to_s4Earthling
    "Although I think I can think, I can't resist biting in hooks."
  end
end

class Moonling < Being
  def to_s4Being
    "My name is Janneke Maan, and apparently some Earthlings will pay me a visit."
  end
end

diverseCollection=[]
diverseCollection << (marsian=Being.new("I come from Mars and like playing hide and seek."))
diverseCollection << (me=Mammal.new(:human))
diverseCollection << (nemo=Fish.new(0.99))
diverseCollection << (jannakeMaan=Moonling.new)

puts "BEGIN ORIGINAL DIVERSE COLLECTION"
diverseCollection.each do |being|
  puts "",being.to_s
end
puts "END ORIGINAL DIVERSE COLLECTION"
puts "\n"+"*"*50+"\n\n"

#Marshal the diverse Array of beings
File.open('diverseCollection.bin','w') do |fo|
  fo << Marshal.dump(diverseCollection)
end

#load the Array of diverse beings
sameDiverseCollection=Marshal.load(File.read('diverseCollection.bin'))

puts "BEGIN LOADED DIVERSE COLLECTION"
puts(
     sameDiverseCollection.collect do |being|
       being.to_s
     end.join("\n\n")
     )
puts "END LOADED DIVERSE COLLECTION"
