functor Make(Stream : sig type t end) : sig
    con t :: Type -> Type

    val mreturn : a ::: Type -> a -> t a
    val mbind : a ::: Type -> b ::: Type ->
                (t a) -> (a -> t b) -> (t b)
    val monad_parse : monad t

    val parse : a ::: Type -> t a -> Stream.t -> option a

    (** Combinators *)
    val fail : a ::: Type -> t a
    val or : a ::: Type -> t a -> t a -> t a
    val maybe : a ::: Type -> t a -> t (option a)
    val maybe' : a ::: Type -> t a -> t unit
    val many : a ::: Type -> t a -> t (list a)
    val count : a ::: Type -> int -> t a -> t (list a)
    val skipMany : a ::: Type -> t a -> t unit
    val sepBy : a ::: Type -> s ::: Type -> t a -> t s -> t (list a)
end

structure String : sig
    con t :: Type -> Type
    val monad_parse : monad t

    val parse : a ::: Type -> t a -> string -> option a

    (** Combinators *)
    val fail : a ::: Type -> t a
    val or : a ::: Type -> t a -> t a -> t a
    val maybe : a ::: Type -> t a -> t (option a)
    val maybe' : a ::: Type -> t a -> t unit
    val many : a ::: Type -> t a -> t (list a)
    val count : a ::: Type -> int -> t a -> t (list a)
    val skipMany : a ::: Type -> t a -> t unit
    val sepBy : a ::: Type -> s ::: Type -> t a -> t s -> t (list a)

    val eof : t unit
    (* We provide alternative versions of some of these predicates
     * that return t unit as a monadic syntactical convenience. *)
    val string : string -> t string
    val string' : string -> t unit
    val stringCI : string -> t string
    val stringCI' : string -> t unit
    val char : char -> t char
    val char' : char -> t unit
    val take : int -> t (string*int)
    val drop : int -> t unit
    val satisfy : (char -> bool) -> t char
    val skip : (char -> bool) -> t unit
    val skipWhile : (char -> bool) -> t unit
    val takeWhile : (char -> bool) -> t (string*int)
    val takeWhile' : (char -> bool) -> t string (* conses *)
    (* Well, "till" is the correct form; but "til" is in common enough
     * usage that I'll prefer it for terseness. *)
    val takeTil : (char -> bool) -> t (string*int)
    val takeTil' : (char -> bool) -> t string (* conses *)
    val takeRest : t string

    (** Convenience functions *)
    val skipSpace : t unit
    val endOfLine : t unit
    val unsigned_int_of_radix : int -> t int
    (*
     * val signed_int_of_radix : int -> t int
     * val double : t float
     *)
end

structure Blob : sig
    con t :: Type -> Type
    val monad_parse : monad t

    val parse : a ::: Type -> t a -> blob -> option a

    (** Combinators *)
    val fail : a ::: Type -> t a
    val or : a ::: Type -> t a -> t a -> t a
    val maybe : a ::: Type -> t a -> t (option a)
    val maybe' : a ::: Type -> t a -> t unit
    val many : a ::: Type -> t a -> t (list a)
    val count : a ::: Type -> int -> t a -> t (list a)
    val skipMany : a ::: Type -> t a -> t unit
    val sepBy : a ::: Type -> s ::: Type -> t a -> t s -> t (list a)
end
