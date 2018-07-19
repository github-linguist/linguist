declare
  class T from ObjectSupport.reflect
     meth init
        skip
     end

     meth name($)
        'T'
     end
  end

  class S from T
     attr a
     feat f

     meth name($)
        'S'
     end

     meth getA($) @a end
     meth setA(V) a := V end
  end

  Obj = {New S init}
  Copy = {Obj clone($)}
in
  %% Some assertions:

  %% Copy is really an S:
  {Copy name($)} = 'S'

  %% Copy is not just a reference to the same object:
  {System.eq Obj Copy} = false

  %% Not a deep copy. Feature f has the same identity for both objects:
  {System.eq Obj.f Copy.f} = true

  %% However, both have their own distinct attributes:
  {Obj setA(13)}
  {Copy setA(14)}
  {Obj getA($)} \= {Copy getA($)} = true
