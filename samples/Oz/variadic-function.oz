declare
  class Demo from BaseObject
     meth test(...)=Msg
        {Record.forAll Msg Show}
     end
  end

  D = {New Demo noop}
  Constructed = {List.toTuple test {List.number 1 10 1}}
in
  {D test(1 2 3 4)}
  {D Constructed}
