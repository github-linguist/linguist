
structure LazyBase:> LAZY_BASE =
   struct
      type 'a lazy = unit -> 'a

      exception Undefined

      fun delay f = f

      fun force f = f()

      val undefined = fn () => raise Undefined
   end

structure LazyMemoBase:> LAZY_BASE =
   struct 

      datatype 'a susp = NotYet of unit -> 'a
                       | Done of 'a

      type 'a lazy = unit -> 'a susp ref

      exception Undefined

      fun delay f = 
          let 
             val r = ref (NotYet f)
          in
             fn () => r
          end

      fun force f = 
          case f() of
             ref (Done x) => x
           | r as ref (NotYet f') =>
             let
                val a = f'()
             in
                r := Done a
              ; a
             end

      val undefined = fn () => raise Undefined
   end

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

structure Lazy' = LazyFn(LazyBase)
structure LazyMemo = LazyFn(LazyMemoBase)
