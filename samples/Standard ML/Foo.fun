
functor LazyFn(B: LAZY_BASE): LAZY' =
   struct

      open B

      fun inject x = delay (fn () => x)

      fun isUndefined x =
          (ignore (force x)
         ; false)
          handle Undefined => true
                              
      fun toString f x = if isUndefined x then "_|_" else f (force x)

      fun eqBy p (x,y) = p(force x,force y)
      fun eq (x,y) = eqBy op= (x,y)
      fun compare p (x,y) = p(force x,force y)

      structure Ops = 
         struct 
            val ! = force
            val ? = inject
         end

      fun map f x = delay (fn () => f (force x))

   end
