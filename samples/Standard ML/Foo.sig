 
signature LAZY_BASE =
   sig 
      type 'a lazy
      exception Undefined
      val force: 'a lazy -> 'a
      val delay: (unit -> 'a) -> 'a lazy
      val undefined: 'a lazy
   end

signature LAZY' =
   sig
      include LAZY_BASE
      val isUndefined: 'a lazy -> bool
      val inject : 'a -> 'a lazy
      val toString: ('a -> string) -> 'a lazy -> string
      val eq: ''a lazy * ''a lazy -> bool
      val eqBy: ('a * 'a -> bool) -> 'a lazy * 'a lazy -> bool
      val compare: ('a * 'a -> order) -> 'a lazy * 'a lazy -> order
      val map: ('a -> 'b) -> 'a lazy -> 'b lazy

      structure Ops: 
                   sig
                      val ! : 'a lazy -> 'a (* force *)
                      val ? : 'a -> 'a lazy (* inject *)
                   end
   end
