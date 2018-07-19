declare
  People = [person(name:joe value:3)
            person(name:bill value:4)
            person(name:alice value:20)
            person(name:harry value:3)]

  SortedPeople = {Sort People
                  fun {$ P1 P2}
                     P1.name < P2.name
                  end
                 }
in
  {ForAll SortedPeople Show}
