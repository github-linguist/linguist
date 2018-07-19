module FiveDogs
  dog = "Benjamin"
  dOg = "Dogley"
  doG = "Fido"
  Dog = "Samba"   # this constant is FiveDogs::Dog
  DOG = "Bernie"  # this constant is FiveDogs::DOG

  names = [dog, dOg, doG, Dog, DOG]
  names.uniq!
  puts "There are %d dogs named %s." % [names.length, names.join(", ")]
  puts
  puts "The local variables are %s." % local_variables.join(", ")
  puts "The constants are %s." % constants.join(", ")
end
