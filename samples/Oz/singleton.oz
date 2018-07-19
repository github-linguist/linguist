declare
   local
      class Singleton
	 meth init
	    skip
	 end
      end
      L = {NewLock}
      Instance
   in
      fun {GetInstance}
	 lock L then
	    if {IsFree Instance} then
	       Instance = {New Singleton init}
	    end
	    Instance
	 end
      end
   end
