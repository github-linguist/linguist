function CreatePet(options)
  local name=options.name
  local species=options.species
  local breed=options.breed
  print('Created a '..breed..' '..species..' named '..name)
end
CreatePet{name='Rex',species='Dog',breed='Irish Setter'}
--position does not matter here.
