(* MLton COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright (C) 1999-2013 Henry Cejtin,
 *                         Matthew Fluet,
 *                         Suresh Jagannathan, and
 *                         Stephen Weeks.
 *
 * Copyright (C) 1997-2000 by the NEC Research Institute
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both the copyright notice and this permission notice and warranty
 * disclaimer appear in supporting documentation, and that the name of
 * the above copyright holders, or their entities, not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * The above copyright holders disclaim all warranties with regard to
 * this software, including all implied warranties of merchantability and
 * fitness. In no event shall the above copyright holders be liable for
 * any special, indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits, whether in an
 * action of contract, negligence or other tortious action, arising out
 * of or in connection with the use or performance of this software.
 *)

functor SsaTree (S: SSA_TREE_STRUCTS): SSA_TREE =
struct

open S

structure Type =
   struct
      datatype t =
         T of {hash: Word.t,
               plist: PropertyList.t,
               tree: tree}
      and tree =
          Array of t
        | CPointer
        | Datatype of Tycon.t
        | IntInf
        | Real of RealSize.t
        | Ref of t
        | Thread
        | Tuple of t vector
        | Vector of t
        | Weak of t
        | Word of WordSize.t

      local
         fun make f (T r) = f r
      in
         val hash = make #hash
         val plist = make #plist
         val tree = make #tree
      end

      datatype dest = datatype tree

      val dest = tree

      fun equals (t, t') = PropertyList.equals (plist t, plist t')

      local
         fun make (sel : dest -> 'a option) =
            let
               val deOpt: t -> 'a option = fn t => sel (dest t)
               val de: t -> 'a = valOf o deOpt
               val is: t -> bool = isSome o deOpt
            in
               (deOpt, de, is)
            end
      in
         val (_,deArray,_) = make (fn Array t => SOME t | _ => NONE)
         val (_,deDatatype,_) = make (fn Datatype tyc => SOME tyc | _ => NONE)
         val (_,deRef,_) = make (fn Ref t => SOME t | _ => NONE)
         val (deTupleOpt,deTuple,isTuple) = make (fn Tuple ts => SOME ts | _ => NONE)
         val (_,deVector,_) = make (fn Vector t => SOME t | _ => NONE)
         val (_,deWeak,_) = make (fn Weak t => SOME t | _ => NONE)
      end

      local
         val same: tree * tree -> bool =
            fn (Array t1, Array t2) => equals (t1, t2)
             | (CPointer, CPointer) => true
             | (Datatype t1, Datatype t2) => Tycon.equals (t1, t2)
             | (IntInf, IntInf) => true
             | (Real s1, Real s2) => RealSize.equals (s1, s2)
             | (Ref t1, Ref t2) => equals (t1, t2)
             | (Thread, Thread) => true
             | (Tuple ts1, Tuple ts2) => Vector.equals (ts1, ts2, equals)
             | (Vector t1, Vector t2) => equals (t1, t2)
             | (Weak t1, Weak t2) => equals (t1, t2)
             | (Word s1, Word s2) => WordSize.equals (s1, s2)
             | _ => false
         val table: t HashSet.t = HashSet.new {hash = hash}
      in
         fun lookup (hash, tr) =
            HashSet.lookupOrInsert (table, hash,
                                    fn t => same (tr, tree t),
                                    fn () => T {hash = hash,
                                                plist = PropertyList.new (),
                                                tree = tr})

         fun stats () =
            let open Layout
            in align [seq [str "num types in hash table = ",
                           Int.layout (HashSet.size table)],
                      Control.sizeMessage ("types hash table", lookup)]
            end
      end

      val newHash = Random.word

      local
         fun make f : t -> t =
            let
               val w = newHash ()
            in
               fn t => lookup (Word.xorb (w, hash t), f t)
            end
      in
         val array = make Array
         val reff = make Ref
         val vector = make Vector
         val weak = make Weak
      end

      val datatypee: Tycon.t -> t =
         fn t => lookup (Tycon.hash t, Datatype t)

      val bool = datatypee Tycon.bool

      local
         fun make (tycon, tree) = lookup (Tycon.hash tycon, tree)
      in
         val cpointer = make (Tycon.cpointer, CPointer)
         val intInf = make (Tycon.intInf, IntInf)
         val thread = make (Tycon.thread, Thread)
      end

      val real: RealSize.t -> t =
         fn s => lookup (Tycon.hash (Tycon.real s), Real s)

      val word: WordSize.t -> t =
         fn s => lookup (Tycon.hash (Tycon.word s), Word s)


      local
         val generator: Word.t = 0wx5555
         val w = newHash ()
      in
         fun tuple ts =
            if 1 = Vector.length ts
               then Vector.sub (ts, 0)
            else lookup (Vector.fold (ts, w, fn (t, w) =>
                                      Word.xorb (w * generator, hash t)),
                         Tuple ts)
      end

      fun ofConst c =
         let
            datatype z = datatype Const.t
         in
            case c of
               IntInf _ => intInf
             | Null => cpointer
             | Real r => real (RealX.size r)
             | Word w => word (WordX.size w)
             | WordVector v => vector (word (WordXVector.elementSize v))
         end

      val unit: t = tuple (Vector.new0 ())

      val isUnit: t -> bool =
         fn t =>
         case deTupleOpt t of
            SOME ts => Vector.isEmpty ts
          | _ => false

      local
         open Layout
      in
         val {get = layout, ...} =
            Property.get
            (plist,
             Property.initRec
             (fn (t, layout) =>
              case dest t of
                 Array t => seq [layout t, str " array"]
               | CPointer => str "pointer"
               | Datatype t => Tycon.layout t
               | IntInf => str "intInf"
               | Real s => str (concat ["real", RealSize.toString s])
               | Ref t => seq [layout t, str " ref"]
               | Thread => str "thread"
               | Tuple ts =>
                    if Vector.isEmpty ts
                       then str "unit"
                    else paren (seq (separate (Vector.toListMap (ts, layout),
                                               " * ")))
               | Vector t => seq [layout t, str " vector"]
               | Weak t => seq [layout t, str " weak"]
               | Word s => str (concat ["word", WordSize.toString s])))
      end

      fun checkPrimApp {args, prim, result, targs}: bool =
         let
            exception BadPrimApp
            fun default () =
               Prim.checkApp
               (prim,
                {args = args,
                 result = result,
                 targs = targs,
                 typeOps = {array = array,
                            arrow = fn _ => raise BadPrimApp,
                            bool = bool,
                            cpointer = cpointer,
                            equals = equals,
                            exn = unit,
                            intInf = intInf,
                            real = real,
                            reff = reff,
                            thread = thread,
                            unit = unit,
                            vector = vector,
                            weak = weak,
                            word = word}})
            val default = fn () =>
               (default ()) handle BadPrimApp => false

            datatype z = datatype Prim.Name.t
         in
            case Prim.name prim of
               _ => default ()
         end
   end

structure Cases =
   struct
      datatype t =
         Con of (Con.t * Label.t) vector
       | Word of WordSize.t * (WordX.t * Label.t) vector

      fun equals (c1: t, c2: t): bool =
         let
            fun doit (l1, l2, eq') =
               Vector.equals
               (l1, l2, fn ((x1, a1), (x2, a2)) =>
                eq' (x1, x2) andalso Label.equals (a1, a2))
         in
            case (c1, c2) of
               (Con l1, Con l2) => doit (l1, l2, Con.equals)
             | (Word (_, l1), Word (_, l2)) => doit (l1, l2, WordX.equals)
             | _ => false
         end

      fun hd (c: t): Label.t =
         let
            fun doit v =
               if Vector.length v >= 1
                  then let val (_, a) = Vector.sub (v, 0)
                       in a
                       end
               else Error.bug "SsaTree.Cases.hd"
         in
            case c of
               Con cs => doit cs
             | Word (_, cs) => doit cs
         end

      fun isEmpty (c: t): bool =
         let
            fun doit v = 0 = Vector.length v
         in
            case c of
               Con cs => doit cs
             | Word (_, cs) => doit cs
         end

      fun fold (c: t, b, f) =
         let
            fun doit l = Vector.fold (l, b, fn ((_, a), b) => f (a, b))
         in
            case c of
               Con l => doit l
             | Word (_, l) => doit l
         end

      fun map (c: t, f): t =
         let
            fun doit l = Vector.map (l, fn (i, x) => (i, f x))
         in
            case c of
               Con l => Con (doit l)
             | Word (s, l) => Word (s, doit l)
         end

      fun forall (c: t, f: Label.t -> bool): bool =
         let
            fun doit l = Vector.forall (l, fn (_, x) => f x)
         in
            case c of
               Con l => doit l
             | Word (_, l) => doit l
         end

      fun length (c: t): int = fold (c, 0, fn (_, i) => i + 1)

      fun foreach (c, f) = fold (c, (), fn (x, ()) => f x)
   end

local open Layout
in
   fun layoutTuple xs = Vector.layout Var.layout xs
end

structure Var =
   struct
      open Var

      fun pretty (x, global) =
         case global x of
            NONE => toString x
          | SOME s => s

      fun prettys (xs: Var.t vector, global: Var.t -> string option) =
         Layout.toString (Vector.layout
                          (fn x =>
                           case global x of
                              NONE => layout x
                            | SOME s => Layout.str s)
                          xs)
   end

structure Exp =
   struct
      datatype t =
         ConApp of {con: Con.t,
                    args: Var.t vector}
       | Const of Const.t
       | PrimApp of {prim: Type.t Prim.t,
                     targs: Type.t vector,
                     args: Var.t vector}
       | Profile of ProfileExp.t
       | Select of {tuple: Var.t,
                    offset: int}
       | Tuple of Var.t vector
       | Var of Var.t

      val unit = Tuple (Vector.new0 ())

      fun foreachVar (e, v) =
         let
            fun vs xs = Vector.foreach (xs, v)
         in
            case e of
               ConApp {args, ...} => vs args
             | Const _ => ()
             | PrimApp {args, ...} => vs args
             | Profile _ => ()
             | Select {tuple, ...} => v tuple
             | Tuple xs => vs xs
             | Var x => v x
         end

      fun replaceVar (e, fx) =
         let
            fun fxs xs = Vector.map (xs, fx)
         in
            case e of
               ConApp {con, args} => ConApp {con = con, args = fxs args}
             | Const _ => e
             | PrimApp {prim, targs, args} =>
                  PrimApp {prim = prim, targs = targs, args = fxs args}
             | Profile _ => e
             | Select {tuple, offset} =>
                  Select {tuple = fx tuple, offset = offset}
             | Tuple xs => Tuple (fxs xs)
             | Var x => Var (fx x)
         end

      fun layout e =
         let
            open Layout
         in
            case e of
               ConApp {con, args} =>
                  seq [Con.layout con, str " ", layoutTuple args]
             | Const c => Const.layout c
             | PrimApp {prim, targs, args} =>
                  seq [Prim.layout prim,
                       if !Control.showTypes
                          then if 0 = Vector.length targs
                                  then empty
                               else Vector.layout Type.layout targs
                       else empty,
                       seq [str " ", layoutTuple args]]
             | Profile p => ProfileExp.layout p
             | Select {tuple, offset} =>
                  seq [str "#", Int.layout offset, str " ",
                       Var.layout tuple]
             | Tuple xs => layoutTuple xs
             | Var x => Var.layout x
         end

      fun maySideEffect (e: t): bool =
         case e of
            ConApp _ => false
          | Const _ => false
          | PrimApp {prim,...} => Prim.maySideEffect prim
          | Profile _ => false
          | Select _ => false
          | Tuple _ => false
          | Var _ => false

      fun varsEquals (xs, xs') = Vector.equals (xs, xs', Var.equals)

      fun equals (e: t, e': t): bool =
         case (e, e') of
            (ConApp {con, args}, ConApp {con = con', args = args'}) =>
               Con.equals (con, con') andalso varsEquals (args, args')
          | (Const c, Const c') => Const.equals (c, c')
          | (PrimApp {prim, args, ...},
             PrimApp {prim = prim', args = args', ...}) =>
               Prim.equals (prim, prim') andalso varsEquals (args, args')
          | (Profile p, Profile p') => ProfileExp.equals (p, p')
          | (Select {tuple = t, offset = i}, Select {tuple = t', offset = i'}) =>
               Var.equals (t, t') andalso i = i'
          | (Tuple xs, Tuple xs') => varsEquals (xs, xs')
          | (Var x, Var x') => Var.equals (x, x')
          | _ => false

      local
         val newHash = Random.word
         val primApp = newHash ()
         val profile = newHash ()
         val select = newHash ()
         val tuple = newHash ()
         fun hashVars (xs: Var.t vector, w: Word.t): Word.t =
            Vector.fold (xs, w, fn (x, w) => Word.xorb (w, Var.hash x))
      in
         val hash: t -> Word.t =
            fn ConApp {con, args, ...} => hashVars (args, Con.hash con)
             | Const c => Const.hash c
             | PrimApp {args, ...} => hashVars (args, primApp)
             | Profile p => Word.xorb (profile, ProfileExp.hash p)
             | Select {tuple, offset} =>
                  Word.xorb (select, Var.hash tuple + Word.fromInt offset)
             | Tuple xs => hashVars (xs, tuple)
             | Var x => Var.hash x
      end

      val hash = Trace.trace ("SsaTree.Exp.hash", layout, Word.layout) hash

      fun toPretty (e: t, global: Var.t -> string option): string =
         case e of
            ConApp {con, args} =>
               concat [Con.toString con, " ", Var.prettys (args, global)]
          | Const c => Const.toString c
          | PrimApp {prim, args, ...} =>
               Layout.toString
               (Prim.layoutApp (prim, args, fn x =>
                                case global x of
                                   NONE => Var.layout x
                                 | SOME s => Layout.str s))
          | Profile p => ProfileExp.toString p
          | Select {tuple, offset} =>
               concat ["#", Int.toString offset, " ", Var.toString tuple]
          | Tuple xs => Var.prettys (xs, global)
          | Var x => Var.toString x
   end
datatype z = datatype Exp.t

structure Statement =
   struct
      datatype t = T of {var: Var.t option,
                         ty: Type.t,
                         exp: Exp.t}

      local
         fun make f (T r) = f r
      in
         val var = make #var
         val exp = make #exp
      end

      fun layout (T {var, ty, exp}) =
         let
            open Layout
         in
            seq [seq [case var of
                         NONE => empty
                       | SOME var =>
                            seq [Var.layout var,
                                 if !Control.showTypes
                                    then seq [str ": ", Type.layout ty]
                                 else empty,
                                 str " = "]],
                 Exp.layout exp]
         end

      local
         fun make f x =
            T {var = NONE,
               ty = Type.unit,
               exp = f x}
      in
         val profile = make Exp.Profile
      end

      fun clear s = Option.app (var s, Var.clear)

      fun prettifyGlobals (v: t vector): Var.t -> string option =
         let
            val {get = global: Var.t -> string option, set = setGlobal, ...} =
               Property.getSet (Var.plist, Property.initConst NONE)
            val _ =
               Vector.foreach
               (v, fn T {var, exp, ...} =>
                Option.app
                (var, fn var =>
                 let
                    fun set s =
                       let
                          val maxSize = 10
                          val s =
                             if String.size s > maxSize
                                then concat [String.prefix (s, maxSize), "..."]
                             else s
                       in
                          setGlobal (var, SOME s)
                       end
                 in
                    case exp of
                       Const c => set (Layout.toString (Const.layout c))
                     | ConApp {con, args, ...} =>
                          if Vector.isEmpty args
                             then set (Con.toString con)
                          else set (concat [Con.toString con, "(...)"])
                     | _ => ()
                 end))
         in
            global
         end
   end

structure Handler =
   struct
      structure Label = Label

      datatype t =
         Caller
       | Dead
       | Handle of Label.t

      fun layout (h: t): Layout.t =
         let
            open Layout
         in
            case h of
               Caller => str "Caller"
             | Dead => str "Dead"
             | Handle l => seq [str "Handle ", Label.layout l]
         end

      val equals =
         fn (Caller, Caller) => true
          | (Dead, Dead) => true
          | (Handle l, Handle l') => Label.equals (l, l')
          | _ => false

      fun foldLabel (h: t, a: 'a, f: Label.t * 'a -> 'a): 'a =
         case h of
            Caller => a
          | Dead => a
          | Handle l => f (l, a)

      fun foreachLabel (h, f) = foldLabel (h, (), f o #1)

      fun map (h, f) =
         case h of
            Caller => Caller
          | Dead => Dead
          | Handle l => Handle (f l)

      local
         val newHash = Random.word
         val caller = newHash ()
         val dead = newHash ()
         val handlee = newHash ()
      in
         fun hash (h: t): word =
            case h of
               Caller => caller
             | Dead => dead
             | Handle l => Word.xorb (handlee, Label.hash l)
      end
   end

structure Return =
   struct
      structure Label = Label
      structure Handler = Handler

      datatype t =
         Dead
       | NonTail of {cont: Label.t,
                     handler: Handler.t}
       | Tail

      fun layout r =
         let
            open Layout
         in
            case r of
               Dead => str "Dead"
             | NonTail {cont, handler} =>
                  seq [str "NonTail ",
                       Layout.record
                       [("cont", Label.layout cont),
                        ("handler", Handler.layout handler)]]
             | Tail => str "Tail"
         end

      fun equals (r, r'): bool =
         case (r, r') of
            (Dead, Dead) => true
          | (NonTail {cont = c, handler = h},
             NonTail {cont = c', handler = h'}) =>
               Label.equals (c, c') andalso Handler.equals (h, h')
           | (Tail, Tail) => true
           | _ => false

      fun foldLabel (r: t, a, f) =
         case r of
            Dead => a
          | NonTail {cont, handler} =>
               Handler.foldLabel (handler, f (cont, a), f)
          | Tail => a

      fun foreachLabel (r, f) = foldLabel (r, (), f o #1)

      fun foreachHandler (r, f) =
         case r of
            Dead => ()
          | NonTail {handler, ...} => Handler.foreachLabel (handler, f)
          | Tail => ()

      fun map (r, f) =
         case r of
            Dead => Dead
          | NonTail {cont, handler} =>
               NonTail {cont = f cont,
                        handler = Handler.map (handler, f)}
          | Tail => Tail

      fun compose (r, r') =
         case r' of
            Dead => Dead
          | NonTail {cont, handler} =>
               NonTail
               {cont = cont,
                handler = (case handler of
                              Handler.Caller =>
                                 (case r of
                                     Dead => Handler.Caller
                                   | NonTail {handler, ...} => handler
                                   | Tail => Handler.Caller)
                            | Handler.Dead => handler
                            | Handler.Handle _ => handler)}
          | Tail => r

      local
         val newHash = Random.word
         val dead = newHash ()
         val nonTail = newHash ()
         val tail = newHash ()
      in
         fun hash r =
            case r of
               Dead => dead
             | NonTail {cont, handler} =>
                  Word.xorb (Word.xorb (nonTail, Label.hash cont),
                             Handler.hash handler)
             | Tail => tail
      end
   end

structure Transfer =
   struct
      datatype t =
         Arith of {prim: Type.t Prim.t,
                   args: Var.t vector,
                   overflow: Label.t, (* Must be nullary. *)
                   success: Label.t, (* Must be unary. *)
                   ty: Type.t}
       | Bug (* MLton thought control couldn't reach here. *)
       | Call of {args: Var.t vector,
                  func: Func.t,
                  return: Return.t}
       | Case of {test: Var.t,
                  cases: Cases.t,
                  default: Label.t option} (* Must be nullary. *)
       | Goto of {dst: Label.t,
                  args: Var.t vector}
       | Raise of Var.t vector
       | Return of Var.t vector
       | Runtime of {prim: Type.t Prim.t,
                     args: Var.t vector,
                     return: Label.t} (* Must be nullary. *)

      fun foreachFuncLabelVar (t, func: Func.t -> unit, label: Label.t -> unit, var) =
         let
            fun vars xs = Vector.foreach (xs, var)
         in
            case t of
               Arith {args, overflow, success, ...} =>
                  (vars args
                   ; label overflow
                   ; label success)
             | Bug => ()
             | Call {func = f, args, return, ...} =>
                  (func f
                   ; Return.foreachLabel (return, label)
                   ; vars args)
             | Case {test, cases, default, ...} =>
                  (var test
                   ; Cases.foreach (cases, label)
                   ; Option.app (default, label))
             | Goto {dst, args, ...} => (vars args; label dst)
             | Raise xs => vars xs
             | Return xs => vars xs
             | Runtime {args, return, ...} =>
                  (vars args
                   ; label return)
         end

      fun foreachFunc (t, func) =
         foreachFuncLabelVar (t, func, fn _ => (), fn _ => ())

      fun foreachLabelVar (t, label, var) =
         foreachFuncLabelVar (t, fn _ => (), label, var)

      fun foreachLabel (t, j) = foreachLabelVar (t, j, fn _ => ())
      fun foreachVar (t, v) = foreachLabelVar (t, fn _ => (), v)

      fun replaceLabelVar (t, fl, fx) =
         let
            fun fxs xs = Vector.map (xs, fx)
         in
            case t of
               Arith {prim, args, overflow, success, ty} =>
                  Arith {prim = prim,
                         args = fxs args,
                         overflow = fl overflow,
                         success = fl success,
                         ty = ty}
             | Bug => Bug
             | Call {func, args, return} =>
                  Call {func = func,
                        args = fxs args,
                        return = Return.map (return, fl)}
             | Case {test, cases, default} =>
                  Case {test = fx test,
                        cases = Cases.map(cases, fl),
                        default = Option.map(default, fl)}
             | Goto {dst, args} =>
                  Goto {dst = fl dst,
                        args = fxs args}
             | Raise xs => Raise (fxs xs)
             | Return xs => Return (fxs xs)
             | Runtime {prim, args, return} =>
                  Runtime {prim = prim,
                           args = fxs args,
                           return = fl return}
         end

      fun replaceLabel (t, f) = replaceLabelVar (t, f, fn x => x)
      fun replaceVar (t, f) = replaceLabelVar (t, fn l => l, f)

      local open Layout
      in
         fun layoutCase {test, cases, default} =
            let
               fun doit (l, layout) =
                  Vector.toListMap
                  (l, fn (i, l) =>
                   seq [layout i, str " => ", Label.layout l])
               datatype z = datatype Cases.t
               val cases =
                  case cases of
                     Con l => doit (l, Con.layout)
                   | Word (_, l) => doit (l, WordX.layout)
               val cases =
                  case default of
                     NONE => cases
                   | SOME j =>
                        cases @ [seq [str "_ => ", Label.layout j]]
            in
               align [seq [str "case ", Var.layout test, str " of"],
                      indent (alignPrefix (cases, "| "), 2)]
            end

         val layout =
            fn Arith {prim, args, overflow, success, ...} =>
                  seq [Label.layout success, str " ",
                       tuple [Prim.layoutApp (prim, args, Var.layout)],
                       str " Overflow => ",
                       Label.layout overflow, str " ()"]
             | Bug => str "Bug"
             | Call {func, args, return} =>
                  seq [Func.layout func, str " ", layoutTuple args,
                       str " ", Return.layout return]
             | Case arg => layoutCase arg
             | Goto {dst, args} =>
                  seq [Label.layout dst, str " ", layoutTuple args]
             | Raise xs => seq [str "raise ", layoutTuple xs]
             | Return xs =>
                  seq [str "return ",
                       if 1 = Vector.length xs
                          then Var.layout (Vector.sub (xs, 0))
                       else layoutTuple xs]
             | Runtime {prim, args, return} =>
                  seq [Label.layout return, str " ",
                       tuple [Prim.layoutApp (prim, args, Var.layout)]]
      end

      fun varsEquals (xs, xs') = Vector.equals (xs, xs', Var.equals)

      fun equals (e: t, e': t): bool =
         case (e, e') of
            (Arith {prim, args, overflow, success, ...},
             Arith {prim = prim', args = args',
                    overflow = overflow', success = success', ...}) =>
               Prim.equals (prim, prim') andalso
               varsEquals (args, args') andalso
               Label.equals (overflow, overflow') andalso
               Label.equals (success, success')
          | (Bug, Bug) => true
          | (Call {func, args, return},
             Call {func = func', args = args', return = return'}) =>
               Func.equals (func, func') andalso
               varsEquals (args, args') andalso
               Return.equals (return, return')
          | (Case {test, cases, default},
             Case {test = test', cases = cases', default = default'}) =>
               Var.equals (test, test')
               andalso Cases.equals (cases, cases')
               andalso Option.equals (default, default', Label.equals)
          | (Goto {dst, args}, Goto {dst = dst', args = args'}) =>
               Label.equals (dst, dst') andalso
               varsEquals (args, args')
          | (Raise xs, Raise xs') => varsEquals (xs, xs')
          | (Return xs, Return xs') => varsEquals (xs, xs')
          | (Runtime {prim, args, return},
             Runtime {prim = prim', args = args', return = return'}) =>
               Prim.equals (prim, prim') andalso
               varsEquals (args, args') andalso
               Label.equals (return, return')
          | _ => false

      local
         val newHash = Random.word
         val bug = newHash ()
         val raisee = newHash ()
         val return = newHash ()
         fun hashVars (xs: Var.t vector, w: Word.t): Word.t =
            Vector.fold (xs, w, fn (x, w) => Word.xorb (w, Var.hash x))
         fun hash2 (w1: Word.t, w2: Word.t) = Word.xorb (w1, w2)
      in
         val hash: t -> Word.t =
            fn Arith {args, overflow, success, ...} =>
                  hashVars (args, hash2 (Label.hash overflow,
                                         Label.hash success))
             | Bug => bug
             | Call {func, args, return} =>
                  hashVars (args, hash2 (Func.hash func, Return.hash return))
             | Case {test, cases, default} =>
                  hash2 (Var.hash test,
                         Cases.fold
                         (cases,
                          Option.fold
                          (default, 0wx55555555,
                           fn (l, w) =>
                           hash2 (Label.hash l, w)),
                          fn (l, w) =>
                          hash2 (Label.hash l, w)))
             | Goto {dst, args} =>
                  hashVars (args, Label.hash dst)
             | Raise xs => hashVars (xs, raisee)
             | Return xs => hashVars (xs, return)
             | Runtime {args, return, ...} => hashVars (args, Label.hash return)
      end

      val hash = Trace.trace ("SsaTree.Transfer.hash", layout, Word.layout) hash

   end
datatype z = datatype Transfer.t

local
   open Layout
in
   fun layoutFormals (xts: (Var.t * Type.t) vector) =
      Vector.layout (fn (x, t) =>
                    seq [Var.layout x,
                         if !Control.showTypes
                            then seq [str ": ", Type.layout t]
                         else empty])
      xts
end

structure Block =
   struct
      datatype t =
         T of {args: (Var.t * Type.t) vector,
               label: Label.t,
               statements: Statement.t vector,
               transfer: Transfer.t}

      local
         fun make f (T r) = f r
      in
         val args = make #args
         val label = make #label
         val statements = make #statements
         val transfer = make #transfer
      end

      fun layout (T {label, args, statements, transfer}) =
         let
            open Layout
         in
            align [seq [Label.layout label, str " ",
                        Vector.layout (fn (x, t) =>
                                       if !Control.showTypes
                                          then seq [Var.layout x, str ": ",
                                                    Type.layout t]
                                       else Var.layout x) args],
                   indent (align
                           [align
                            (Vector.toListMap (statements, Statement.layout)),
                            Transfer.layout transfer],
                           2)]
         end

      fun clear (T {label, args, statements, ...}) =
         (Label.clear label
          ; Vector.foreach (args, Var.clear o #1)
          ; Vector.foreach (statements, Statement.clear))
   end

structure Datatype =
   struct
      datatype t =
         T of {
               tycon: Tycon.t,
               cons: {con: Con.t,
                      args: Type.t vector} vector
               }

      fun layout (T {tycon, cons}) =
         let
            open Layout
         in
            seq [Tycon.layout tycon,
                 str " = ",
                 alignPrefix
                 (Vector.toListMap
                  (cons, fn {con, args} =>
                   seq [Con.layout con,
                        if Vector.isEmpty args
                           then empty
                        else seq [str " of ",
                                  Vector.layout Type.layout args]]),
                  "| ")]
         end

      fun clear (T {tycon, cons}) =
         (Tycon.clear tycon
          ; Vector.foreach (cons, Con.clear o #con))
   end

structure Function =
   struct
      structure CPromise = ClearablePromise

      type dest = {args: (Var.t * Type.t) vector,
                   blocks: Block.t vector,
                   mayInline: bool,
                   name: Func.t,
                   raises: Type.t vector option,
                   returns: Type.t vector option,
                   start: Label.t}

      (* There is a messy interaction between the laziness used in controlFlow
       * and the property lists on labels because the former stores
       * stuff on the property lists.  So, if you force the laziness, then
       * clear the property lists, then try to use the lazy stuff, you will
       * get screwed with undefined properties.  The right thing to do is reset
       * the laziness when the properties are cleared.
       *)
      datatype t =
         T of {controlFlow:
               {dfsTree: unit -> Block.t Tree.t,
                dominatorTree: unit -> Block.t Tree.t,
                graph: unit DirectedGraph.t,
                labelNode: Label.t -> unit DirectedGraph.Node.t,
                nodeBlock: unit DirectedGraph.Node.t -> Block.t} CPromise.t,
               dest: dest}

      local
         fun make f (T {dest, ...}) = f dest
      in
         val blocks = make #blocks
         val dest = make (fn d => d)
         val mayInline = make #mayInline
         val name = make #name
      end

      fun foreachVar (f: t, fx: Var.t * Type.t -> unit): unit =
         let
            val {args, blocks, ...} = dest f
            val _ = Vector.foreach (args, fx)
            val _ =
               Vector.foreach
               (blocks, fn Block.T {args, statements, ...} =>
                (Vector.foreach (args, fx)
                 ; Vector.foreach (statements, fn Statement.T {var, ty, ...} =>
                                   Option.app (var, fn x => fx (x, ty)))))
         in
            ()
         end

      fun controlFlow (T {controlFlow, ...}) =
         let
            val {graph, labelNode, nodeBlock, ...} = CPromise.force controlFlow
         in
            {graph = graph, labelNode = labelNode, nodeBlock = nodeBlock}
         end

      local
         fun make sel =
            fn T {controlFlow, ...} => sel (CPromise.force controlFlow) ()
      in
         val dominatorTree = make #dominatorTree
      end

      fun dfs (f, v) =
         let
            val {blocks, start, ...} = dest f
            val numBlocks = Vector.length blocks
            val {get = labelIndex, set = setLabelIndex, rem, ...} =
               Property.getSetOnce (Label.plist,
                                    Property.initRaise ("index", Label.layout))
            val _ = Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
                                     setLabelIndex (label, i))
            val visited = Array.array (numBlocks, false)
            fun visit (l: Label.t): unit =
               let
                  val i = labelIndex l
               in
                  if Array.sub (visited, i)
                     then ()
                  else
                     let
                        val _ = Array.update (visited, i, true)
                        val b as Block.T {transfer, ...} =
                           Vector.sub (blocks, i)
                        val v' = v b
                        val _ = Transfer.foreachLabel (transfer, visit)
                        val _ = v' ()
                     in
                        ()
                     end
               end
            val _ = visit start
            val _ = Vector.foreach (blocks, rem o Block.label)
         in
            ()
         end

      local
         structure Graph = DirectedGraph
         structure Node = Graph.Node
         structure Edge = Graph.Edge
      in
         fun determineControlFlow ({blocks, start, ...}: dest) =
            let
               open Dot
               val g = Graph.new ()
               fun newNode () = Graph.newNode g
               val {get = labelNode, ...} =
                  Property.get
                  (Label.plist, Property.initFun (fn _ => newNode ()))
               val {get = nodeInfo: unit Node.t -> {block: Block.t},
                    set = setNodeInfo, ...} =
                  Property.getSetOnce
                  (Node.plist, Property.initRaise ("info", Node.layout))
               val _ =
                  Vector.foreach
                  (blocks, fn b as Block.T {label, transfer, ...} =>
                   let
                      val from = labelNode label
                      val _ = setNodeInfo (from, {block = b})
                      val _ =
                         Transfer.foreachLabel
                         (transfer, fn to =>
                          (ignore o Graph.addEdge)
                          (g, {from = from, to = labelNode to}))
                   in
                      ()
                   end)
               val root = labelNode start
               val dfsTree =
                  Promise.lazy
                  (fn () =>
                   Graph.dfsTree (g, {root = root,
                                      nodeValue = #block o nodeInfo}))
               val dominatorTree =
                  Promise.lazy
                  (fn () =>
                   Graph.dominatorTree (g, {root = root,
                                            nodeValue = #block o nodeInfo}))
            in
               {dfsTree = dfsTree,
                dominatorTree = dominatorTree,
                graph = g,
                labelNode = labelNode,
                nodeBlock = #block o nodeInfo}
            end

         fun layoutDot (f, global: Var.t -> string option) =
            let
               val {name, start, blocks, ...} = dest f
               fun makeName (name: string,
                             formals: (Var.t * Type.t) vector): string =
                  concat [name, " ",
                          let
                             open Layout
                          in
                             toString
                             (vector
                              (Vector.map
                               (formals, fn (var, ty) =>
                                if !Control.showTypes
                                   then seq [Var.layout var,
                                             str ": ",
                                             Type.layout ty]
                                else Var.layout var)))
                          end]
               open Dot
               val graph = Graph.new ()
               val {get = nodeOptions, ...} =
                  Property.get (Node.plist, Property.initFun (fn _ => ref []))
               fun setNodeText (n: unit Node.t, l): unit =
                  List.push (nodeOptions n, NodeOption.Label l)
               fun newNode () = Graph.newNode graph
               val {destroy, get = labelNode} =
                  Property.destGet (Label.plist,
                                    Property.initFun (fn _ => newNode ()))
               val {get = edgeOptions, set = setEdgeOptions, ...} =
                  Property.getSetOnce (Edge.plist, Property.initConst [])
               val _ =
                  Vector.foreach
                  (blocks, fn Block.T {label, args, statements, transfer} =>
                   let
                      val from = labelNode label
                      fun edge (to: Label.t,
                                label: string,
                                style: style): unit =
                         let
                            val e = Graph.addEdge (graph, {from = from,
                                                           to = labelNode to})
                            val _ = setEdgeOptions (e, [EdgeOption.label label,
                                                        EdgeOption.Style style])
                         in
                            ()
                         end
                      val rest =
                         case transfer of
                            Arith {prim, args, overflow, success, ...} =>
                               (edge (success, "", Solid)
                                ; edge (overflow, "Overflow", Dashed)
                                ; [Layout.toString
                                   (Prim.layoutApp (prim, args, fn x =>
                                                    Layout.str
                                                    (Var.pretty (x, global))))])
                          | Bug => ["bug"]
                          | Call {func, args, return} =>
                               let
                                  val f = Func.toString func
                                  val args = Var.prettys (args, global)
                                  val _ =
                                     case return of
                                        Return.Dead => ()
                                      | Return.NonTail {cont, handler} =>
                                           (edge (cont, "", Dotted)
                                            ; (Handler.foreachLabel
                                               (handler, fn l =>
                                                edge (l, "", Dashed))))
                                      | Return.Tail => ()
                               in
                                  [f, " ", args]
                               end
                          | Case {test, cases, default, ...} =>
                               let
                                  fun doit (v, toString) =
                                     Vector.foreach
                                     (v, fn (x, j) =>
                                      edge (j, toString x, Solid))
                                  val _ =
                                     case cases of
                                        Cases.Con v => doit (v, Con.toString)
                                      | Cases.Word (_, v) =>
                                           doit (v, WordX.toString)
                                  val _ =
                                     case default of
                                        NONE => ()
                                      | SOME j =>
                                           edge (j, "default", Solid)
                               in
                                  ["case ", Var.toString test]
                               end
                          | Goto {dst, args} =>
                               (edge (dst, "", Solid)
                                ; [Label.toString dst, " ",
                                   Var.prettys (args, global)])
                          | Raise xs => ["raise ", Var.prettys (xs, global)]
                          | Return xs => ["return ", Var.prettys (xs, global)]
                          | Runtime {prim, args, return} =>
                               (edge (return, "", Solid)
                                ; [Layout.toString
                                   (Prim.layoutApp (prim, args, fn x =>
                                                    Layout.str
                                                    (Var.pretty (x, global))))])
                      val lab =
                         Vector.foldr
                         (statements, [(concat rest, Left)],
                          fn (Statement.T {var, ty, exp, ...}, ac) =>
                          let
                             val exp = Exp.toPretty (exp, global)
                             val s =
                                if Type.isUnit ty
                                   then exp
                                else
                                   case var of
                                      NONE => exp
                                    | SOME var =>
                                         concat [Var.toString var,
                                                 if !Control.showTypes
                                                    then concat [": ",
                                                                 Layout.toString
                                                                 (Type.layout ty)]
                                                 else "",
                                                    " = ", exp]
                          in
                             (s, Left) :: ac
                          end)
                      val name = makeName (Label.toString label, args)
                      val _ = setNodeText (from, (name, Left) :: lab)
                   in
                      ()
                   end)
               val root = labelNode start
               val graphLayout =
                  Graph.layoutDot
                  (graph, fn {nodeName} =>
                   {title = concat [Func.toString name, " control-flow graph"],
                    options = [GraphOption.Rank (Min, [{nodeName = nodeName root}])],
                    edgeOptions = edgeOptions,
                    nodeOptions =
                    fn n => let
                               val l = ! (nodeOptions n)
                               open NodeOption
                            in FontColor Black :: Shape Box :: l
                            end})
               fun treeLayout () =
                  let
                     val {get = nodeOptions, set = setNodeOptions, ...} =
                        Property.getSetOnce (Node.plist, Property.initConst [])
                     val _ =
                        Vector.foreach
                        (blocks, fn Block.T {label, ...} =>
                         setNodeOptions (labelNode label,
                                         [NodeOption.label (Label.toString label)]))
                     val treeLayout =
                        Tree.layoutDot
                        (Graph.dominatorTree (graph,
                                              {root = root,
                                               nodeValue = fn n => n}),
                         {title = concat [Func.toString name, " dominator tree"],
                          options = [],
                          nodeOptions = nodeOptions})
                  in
                     treeLayout
                  end
               (*
               fun loopForestLayout () =
                  let
                     val {get = nodeName, set = setNodeName, ...} =
                        Property.getSetOnce (Node.plist, Property.initConst "")
                     val _ =
                        Vector.foreach
                        (blocks, fn Block.T {label, ...} =>
                         setNodeName (labelNode label, Label.toString label))
                     val loopForestLayout =
                        Graph.LoopForest.layoutDot
                        (Graph.loopForestSteensgaard (graph,
                                                      {root = root}),
                         {title = concat [Func.toString name, " loop forest"],
                          options = [],
                          nodeName = nodeName})
                  in
                     loopForestLayout
                  end
               *)
            in
               {destroy = destroy,
                graph = graphLayout,
                tree = treeLayout}
            end
      end

      fun new (dest: dest) =
         let
            val controlFlow = CPromise.delay (fn () => determineControlFlow dest)
         in
            T {controlFlow = controlFlow,
               dest = dest}
         end

      fun clear (T {controlFlow, dest, ...}) =
         let
            val {args, blocks, ...} = dest
            val _ = (Vector.foreach (args, Var.clear o #1)
                     ; Vector.foreach (blocks, Block.clear))
            val _ = CPromise.clear controlFlow
         in
            ()
         end

      fun layoutHeader (f: t): Layout.t =
         let
            val {args, name, raises, returns, start, ...} = dest f
            open Layout
         in
            seq [str "fun ",
                 Func.layout name,
                 str " ",
                 layoutFormals args,
                 if !Control.showTypes
                    then seq [str ": ",
                              record [("raises",
                                       Option.layout
                                       (Vector.layout Type.layout) raises),
                                      ("returns",
                                       Option.layout
                                       (Vector.layout Type.layout) returns)]]
                 else empty,
                    str " = ", Label.layout start, str " ()"]
         end

      fun layout (f: t) =
         let
            val {blocks, ...} = dest f
            open Layout
         in
            align [layoutHeader f,
                   indent (align (Vector.toListMap (blocks, Block.layout)), 2)]
         end

      fun layouts (f: t, global, output: Layout.t -> unit): unit =
         let
            val {blocks, name, ...} = dest f
            val _ = output (layoutHeader f)
            val _ = Vector.foreach (blocks, fn b =>
                                    output (Layout.indent (Block.layout b, 2)))
            val _ =
               if not (!Control.keepDot)
                  then ()
               else
                  let
                     val {destroy, graph, tree} =
                        layoutDot (f, global)
                     val name = Func.toString name
                     fun doit (s, g) =
                        let
                           open Control
                        in
                           saveToFile
                           ({suffix = concat [name, ".", s, ".dot"]},
                            Dot, (), Layout (fn () => g))
                        end
                     val _ = doit ("cfg", graph)
                        handle _ => Error.warning "SsaTree.layouts: couldn't layout cfg"
                     val _ = doit ("dom", tree ())
                        handle _ => Error.warning "SsaTree.layouts: couldn't layout dom"
                     (*
                     val _ = doit ("lf", loopForest ())
                        handle _ => Error.warning "SsaTree.layouts: couldn't layout lf"
                     *)
                     val () = destroy ()
                  in
                     ()
                  end
         in
            ()
         end

      fun alphaRename f =
         let
            local
               fun make (new, plist) =
                  let
                     val {get, set, destroy, ...} =
                        Property.destGetSetOnce (plist, Property.initConst NONE)
                     fun bind x =
                        let
                           val x' = new x
                           val _ = set (x, SOME x')
                        in
                           x'
                        end
                     fun lookup x =
                        case get x of
                           NONE => x
                         | SOME y => y
                  in (bind, lookup, destroy)
                  end
            in
               val (bindVar, lookupVar, destroyVar) =
                  make (Var.new, Var.plist)
               val (bindLabel, lookupLabel, destroyLabel) =
                  make (Label.new, Label.plist)
            end
            val {args, blocks, mayInline, name, raises, returns, start, ...} =
               dest f
            val args = Vector.map (args, fn (x, ty) => (bindVar x, ty))
            val bindLabel = ignore o bindLabel
            val bindVar = ignore o bindVar
            val _ =
               Vector.foreach
               (blocks, fn Block.T {label, args, statements, ...} =>
                (bindLabel label
                 ; Vector.foreach (args, fn (x, _) => bindVar x)
                 ; Vector.foreach (statements,
                                   fn Statement.T {var, ...} =>
                                   Option.app (var, bindVar))))
            val blocks =
               Vector.map
               (blocks, fn Block.T {label, args, statements, transfer} =>
                Block.T {label = lookupLabel label,
                         args = Vector.map (args, fn (x, ty) =>
                                            (lookupVar x, ty)),
                         statements = Vector.map
                                      (statements,
                                       fn Statement.T {var, ty, exp} =>
                                       Statement.T
                                       {var = Option.map (var, lookupVar),
                                        ty = ty,
                                        exp = Exp.replaceVar
                                              (exp, lookupVar)}),
                         transfer = Transfer.replaceLabelVar
                                    (transfer, lookupLabel, lookupVar)})
            val start = lookupLabel start
            val _ = destroyVar ()
            val _ = destroyLabel ()
         in
            new {args = args,
                 blocks = blocks,
                 mayInline = mayInline,
                 name = name,
                 raises = raises,
                 returns = returns,
                 start = start}
         end

      fun profile (f: t, sourceInfo): t =
         if !Control.profile = Control.ProfileNone
            orelse !Control.profileIL <> Control.ProfileSource
            then f
         else
         let
            val _ = Control.diagnostic (fn () => layout f)
            val {args, blocks, mayInline, name, raises, returns, start} = dest f
            val extraBlocks = ref []
            val {get = labelBlock, set = setLabelBlock, rem} =
               Property.getSetOnce
               (Label.plist, Property.initRaise ("block", Label.layout))
            val _ =
               Vector.foreach
               (blocks, fn block as Block.T {label, ...} =>
                setLabelBlock (label, block))
            val blocks =
               Vector.map
               (blocks, fn Block.T {args, label, statements, transfer} =>
                let
                   fun make (exp: Exp.t): Statement.t =
                      Statement.T {exp = exp,
                                   ty = Type.unit,
                                   var = NONE}
                   val statements =
                      if Label.equals (label, start)
                         then (Vector.concat
                               [Vector.new1
                                (make (Exp.Profile
                                       (ProfileExp.Enter sourceInfo))),
                                statements])
                      else statements
                   fun leave () =
                      make (Exp.Profile (ProfileExp.Leave sourceInfo))
                   fun prefix (l: Label.t,
                               statements: Statement.t vector): Label.t =
                      let
                         val Block.T {args, ...} = labelBlock l
                         val c = Label.newNoname ()
                         val xs = Vector.map (args, fn (x, _) => Var.new x)
                         val _ =
                            List.push
                            (extraBlocks,
                             Block.T
                             {args = Vector.map2 (xs, args, fn (x, (_, t)) =>
                                                  (x, t)),
                              label = c,
                              statements = statements,
                              transfer = Goto {args = xs,
                                               dst = l}})
                      in
                         c
                      end
                   fun genHandler (cont: Label.t)
                      : Statement.t vector * Label.t * Handler.t =
                      case raises of
                         NONE => (statements, cont, Handler.Caller)
                       | SOME ts =>
                            let
                               val xs = Vector.map (ts, fn _ => Var.newNoname ())
                               val l = Label.newNoname ()
                               val _ =
                                  List.push
                                  (extraBlocks,
                                   Block.T
                                   {args = Vector.zip (xs, ts),
                                    label = l,
                                    statements = Vector.new1 (leave ()),
                                    transfer = Transfer.Raise xs})
                            in
                               (statements,
                                prefix (cont, Vector.new0 ()),
                                Handler.Handle l)
                            end
                   fun addLeave () =
                      (Vector.concat [statements,
                                      Vector.new1 (leave ())],
                       transfer)
                   val (statements, transfer) =
                      case transfer of
                         Call {args, func, return} =>
                            let
                               datatype z = datatype Return.t
                            in
                               case return of
                                  Dead => (statements, transfer)
                                | NonTail {cont, handler} =>
                                     (case handler of
                                         Handler.Dead => (statements, transfer)
                                       | Handler.Caller =>
                                            let
                                               val (statements, cont, handler) =
                                                  genHandler cont
                                               val return =
                                                  Return.NonTail
                                                  {cont = cont,
                                                   handler = handler}
                                            in
                                               (statements,
                                                Call {args = args,
                                                      func = func,
                                                      return = return})
                                            end
                                       | Handler.Handle _ =>
                                            (statements, transfer))
                                | Tail => addLeave ()
                            end
                       | Raise _ => addLeave ()
                       | Return _ => addLeave ()
                       | _ => (statements, transfer)
                in
                   Block.T {args = args,
                            label = label,
                            statements = statements,
                            transfer = transfer}
                end)
            val _ = Vector.foreach (blocks, rem o Block.label)
            val blocks = Vector.concat [Vector.fromList (!extraBlocks), blocks]
            val f =
               new {args = args,
                    blocks = blocks,
                    mayInline = mayInline,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
            val _ = Control.diagnostic (fn () => layout f)
         in
            f
         end

      val profile =
         Trace.trace2 ("SsaTree.Function.profile", layout, SourceInfo.layout, layout)
         profile
   end

structure Program =
   struct
      datatype t =
         T of {
               datatypes: Datatype.t vector,
               globals: Statement.t vector,
               functions: Function.t list,
               main: Func.t
               }
   end

structure Program =
   struct
      open Program

      local
         structure Graph = DirectedGraph
         structure Node = Graph.Node
         structure Edge = Graph.Edge
      in
         fun layoutCallGraph (T {functions, main, ...},
                              title: string): Layout.t =
            let
               open Dot
               val graph = Graph.new ()
               val {get = nodeOptions, set = setNodeOptions, ...} =
                  Property.getSetOnce
                  (Node.plist, Property.initRaise ("options", Node.layout))
               val {get = funcNode, destroy} =
                  Property.destGet
                  (Func.plist, Property.initFun
                   (fn f =>
                    let
                       val n = Graph.newNode graph
                       val _ =
                          setNodeOptions
                          (n,
                           let open NodeOption
                           in [FontColor Black, label (Func.toString f)]
                           end)
                    in
                       n
                    end))
               val {get = edgeOptions, set = setEdgeOptions, ...} =
                  Property.getSetOnce (Edge.plist, Property.initConst [])
               val _ =
                  List.foreach
                  (functions, fn f =>
                   let
                      val {name, blocks, ...} = Function.dest f
                      val from = funcNode name
                      val {get, destroy} =
                         Property.destGet
                         (Node.plist,
                          Property.initFun (fn _ => {nontail = ref false,
                                                     tail = ref false}))
                      val _ =
                         Vector.foreach
                         (blocks, fn Block.T {transfer, ...} =>
                          case transfer of
                             Call {func, return, ...} =>
                                let
                                   val to = funcNode func
                                   val {tail, nontail} = get to
                                   datatype z = datatype Return.t
                                   val is =
                                      case return of
                                         Dead => false
                                       | NonTail _ => true
                                       | Tail => false
                                   val r = if is then nontail else tail
                                in
                                   if !r
                                      then ()
                                   else (r := true
                                         ; (setEdgeOptions
                                            (Graph.addEdge
                                             (graph, {from = from, to = to}),
                                             if is
                                                then []
                                             else [EdgeOption.Style Dotted])))
                                end
                           | _ => ())
                      val _ = destroy ()
                   in
                      ()
                   end)
               val root = funcNode main
               val l =
                  Graph.layoutDot
                  (graph, fn {nodeName} =>
                   {title = title,
                    options = [GraphOption.Rank (Min, [{nodeName = nodeName root}])],
                    edgeOptions = edgeOptions,
                    nodeOptions = nodeOptions})
               val _ = destroy ()
            in
               l
            end
      end

      fun layouts (p as T {datatypes, globals, functions, main},
                   output': Layout.t -> unit) =
         let
            val global = Statement.prettifyGlobals globals
            open Layout
            (* Layout includes an output function, so we need to rebind output
             * to the one above.
             *)
            val output = output'
         in
            output (str "\n\nDatatypes:")
            ; Vector.foreach (datatypes, output o Datatype.layout)
            ; output (str "\n\nGlobals:")
            ; Vector.foreach (globals, output o Statement.layout)
            ; output (seq [str "\n\nMain: ", Func.layout main])
            ; output (str "\n\nFunctions:")
            ; List.foreach (functions, fn f =>
                            Function.layouts (f, global, output))
            ; if not (!Control.keepDot)
                 then ()
              else
                 let
                    open Control
                 in
                    saveToFile
                    ({suffix = "call-graph.dot"},
                     Dot, (), Layout (fn () =>
                                      layoutCallGraph (p, !Control.inputFile)))
                 end
         end

      fun layoutStats (T {datatypes, globals, functions, main, ...}) =
         let
            val (mainNumVars, mainNumBlocks) =
               case List.peek (functions, fn f =>
                               Func.equals (main, Function.name f)) of
                  NONE => Error.bug "SsaTree.Program.layoutStats: no main"
                | SOME f =>
                     let
                        val numVars = ref 0
                        val _ = Function.foreachVar (f, fn _ => Int.inc numVars)
                        val {blocks, ...} = Function.dest f
                        val numBlocks = Vector.length blocks
                     in
                        (!numVars, numBlocks)
                     end
            val numTypes = ref 0
            val {get = countType, destroy} =
               Property.destGet
               (Type.plist,
                Property.initRec
                (fn (t, countType) =>
                 let
                    datatype z = datatype Type.dest
                    val _ =
                       case Type.dest t of
                          Array t => countType t
                        | CPointer => ()
                        | Datatype _ => ()
                        | IntInf => ()
                        | Real _ => ()
                        | Ref t => countType t
                        | Thread => ()
                        | Tuple ts => Vector.foreach (ts, countType)
                        | Vector t => countType t
                        | Weak t => countType t
                        | Word _ => ()
                    val _ = Int.inc numTypes
                 in
                    ()
                 end))
            val _ =
               Vector.foreach
               (datatypes, fn Datatype.T {cons, ...} =>
                Vector.foreach (cons, fn {args, ...} =>
                                Vector.foreach (args, countType)))
            val numStatements = ref (Vector.length globals)
            val numBlocks = ref 0
            val _ =
               List.foreach
               (functions, fn f =>
                let
                   val {args, blocks, ...} = Function.dest f
                   val _ = Vector.foreach (args, countType o #2)
                   val _ =
                      Vector.foreach
                      (blocks, fn Block.T {args, statements, ...} =>
                       let
                          val _ = Int.inc numBlocks
                          val _ = Vector.foreach (args, countType o #2)
                          val _ =
                             Vector.foreach
                             (statements, fn Statement.T {ty, ...} =>
                              let
                                 val _ = Int.inc numStatements
                                 val _ = countType ty
                              in () end)
                       in () end)
                in () end)
            val numFunctions = List.length functions
            val _ = destroy ()
            open Layout
         in
            align
            [seq [str "num vars in main = ", Int.layout mainNumVars],
             seq [str "num blocks in main = ", Int.layout mainNumBlocks],
             seq [str "num functions in program = ", Int.layout numFunctions],
             seq [str "num blocks in program = ", Int.layout (!numBlocks)],
             seq [str "num statements in program = ", Int.layout (!numStatements)],
             seq [str "num types in program = ", Int.layout (!numTypes)],
             Type.stats ()]
         end

      (* clear all property lists reachable from program *)
      fun clear (T {datatypes, globals, functions, ...}) =
         ((* Can't do Type.clear because it clears out the info needed for
           * Type.dest.
           *)
          Vector.foreach (datatypes, Datatype.clear)
          ; Vector.foreach (globals, Statement.clear)
          ; List.foreach (functions, Function.clear))

      fun clearGlobals (T {globals, ...}) =
         Vector.foreach (globals, Statement.clear)

      fun clearTop (p as T {datatypes, functions, ...}) =
         (Vector.foreach (datatypes, Datatype.clear)
          ; List.foreach (functions, Func.clear o Function.name)
          ; clearGlobals p)

      fun foreachVar (T {globals, functions, ...}, f) =
         (Vector.foreach (globals, fn Statement.T {var, ty, ...} =>
                          f (valOf var, ty))
          ; List.foreach (functions, fn g => Function.foreachVar (g, f)))

      fun foreachPrim (T {globals, functions, ...}, f) =
         let
            fun loopStatement (Statement.T {exp, ...}) =
                case exp of
                   PrimApp {prim, ...} => f prim
                 | _ => ()
             fun loopTransfer t =
                case t of
                   Arith {prim, ...} => f prim
                 | Runtime {prim, ...} => f prim
                 | _ => ()
             val _ = Vector.foreach (globals, loopStatement)
             val _ =
                List.foreach
                (functions, fn f =>
                 Vector.foreach
                 (Function.blocks f, fn Block.T {statements, transfer, ...} =>
                  (Vector.foreach (statements, loopStatement);
                   loopTransfer transfer)))
         in
            ()
         end

      fun hasPrim (p, f) =
         Exn.withEscape
         (fn escape =>
          (foreachPrim (p, fn prim => if f prim then escape true else ())
           ; false))

      fun mainFunction (T {functions, main, ...}) =
         case List.peek (functions, fn f =>
                         Func.equals (main, Function.name f)) of
            NONE => Error.bug "SsaTree.Program.mainFunction: no main function"
          | SOME f => f

      fun dfs (p, v) =
         let
            val T {functions, main, ...} = p
            val functions = Vector.fromList functions
            val numFunctions = Vector.length functions
            val {get = funcIndex, set = setFuncIndex, rem, ...} =
               Property.getSetOnce (Func.plist,
                                    Property.initRaise ("index", Func.layout))
            val _ = Vector.foreachi (functions, fn (i, f) =>
                                     setFuncIndex (#name (Function.dest f), i))
            val visited = Array.array (numFunctions, false)
            fun visit (f: Func.t): unit =
               let
                  val i = funcIndex f
               in
                  if Array.sub (visited, i)
                     then ()
                  else
                     let
                        val _ = Array.update (visited, i, true)
                        val f = Vector.sub (functions, i)
                        val v' = v f
                        val _ = Function.dfs
                                (f, fn Block.T {transfer, ...} =>
                                 (Transfer.foreachFunc (transfer, visit)
                                  ; fn () => ()))
                        val _ = v' ()
                     in
                        ()
                     end
               end
            val _ = visit main
            val _ = Vector.foreach (functions, rem o Function.name)
         in
            ()
         end
   end

end
