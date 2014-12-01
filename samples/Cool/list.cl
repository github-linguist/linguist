(* This simple example of a list class is adapted from an example in the
   Cool distribution. *)

class List {
   isNil() : Bool { true };
   head()  : Int { { abort(); 0; } };
   tail()  : List { { abort(); self; } };
   cons(i : Int) : List {
      (new Cons).init(i, self)
   };
};

class Cons inherits List {
   car : Int;	-- The element in this list cell
   cdr : List;	-- The rest of the list
   isNil() : Bool { false };
   head()  : Int { car };
   tail()  : List { cdr };
   init(i : Int, rest : List) : List {
      {
	 car <- i;
	 cdr <- rest;
	 self;
      }
   };
};
