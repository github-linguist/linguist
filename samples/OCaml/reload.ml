(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Cmm
open Arch
open Reg
open Mach

(* Reloading for the AMD64 *)

(* Summary of instruction set constraints:
   "S" means either stack or register, "R" means register only.
   Operation                    Res     Arg1    Arg2
     Imove                      R       S
                             or S       R
     Iconst_int                 S if 32-bit signed, R otherwise
     Iconst_float               R
     Iconst_symbol (not PIC)    S
     Iconst_symbol (PIC)        R
     Icall_ind                          R
     Itailcall_ind                      R
     Iload                      R       R       R
     Istore                             R       R
     Iintop(Icomp)              R       R       S
                            or  S       S       R
     Iintop(Imul|Idiv|mod)      R       R       S
     Iintop(shift)              S       S       R
     Iintop(others)             R       R       S
                            or  S       S       R
     Iintop_imm(Iadd, n)/lea    R       R
     Iintop_imm(others)         S       S
     Inegf...Idivf              R       R       S
     Ifloatofint                R       S
     Iintoffloat                R       S
     Ispecific(Ilea)            R       R       R
     Ispecific(Ifloatarithmem)  R       R       R

   Conditional branches:
     Iinttest                           S       R
                                    or  R       S
     Ifloattest                         R       S    (or  S R if swapped test)
     other tests                        S
*)

let stackp r =
  match r.loc with
    Stack _ -> true
  | _ -> false

class reload = object (self)

inherit Reloadgen.reload_generic as super

method! reload_operation op arg res =
  match op with
  | Iintop(Iadd|Isub|Iand|Ior|Ixor|Icomp _|Icheckbound) ->
      (* One of the two arguments can reside in the stack, but not both *)
      if stackp arg.(0) && stackp arg.(1)
      then ([|arg.(0); self#makereg arg.(1)|], res)
      else (arg, res)
  | Iintop_imm(Iadd, _) when arg.(0).loc <> res.(0).loc ->
      (* This add will be turned into a lea; args and results must be
         in registers *)
      super#reload_operation op arg res
  | Iintop(Idiv | Imod | Ilsl | Ilsr | Iasr)
  | Iintop_imm(_, _) ->
      (* The argument(s) and results can be either in register or on stack *)
      (* Note: Idiv, Imod: arg(0) and res(0) already forced in regs
               Ilsl, Ilsr, Iasr: arg(1) already forced in regs *)
      (arg, res)
  | Iintop(Imul) | Iaddf | Isubf | Imulf | Idivf ->
      (* First argument (= result) must be in register, second arg
         can reside in the stack *)
      if stackp arg.(0)
      then (let r = self#makereg arg.(0) in ([|r; arg.(1)|], [|r|]))
      else (arg, res)
  | Ifloatofint | Iintoffloat ->
      (* Result must be in register, but argument can be on stack *)
      (arg, (if stackp res.(0) then [| self#makereg res.(0) |] else res))
  | Iconst_int n ->
      if n <= 0x7FFFFFFFn && n >= -0x80000000n
      then (arg, res)
      else super#reload_operation op arg res
  | Iconst_symbol _ ->
      if !pic_code || !Clflags.dlcode
      then super#reload_operation op arg res
      else (arg, res)
  | _ -> (* Other operations: all args and results in registers *)
      super#reload_operation op arg res

method! reload_test tst arg =
  match tst with
    Iinttest cmp ->
      (* One of the two arguments can reside on stack *)
      if stackp arg.(0) && stackp arg.(1)
      then [| self#makereg arg.(0); arg.(1) |]
      else arg
  | Ifloattest((Clt|Cle), _) ->
      (* Cf. emit.mlp: we swap arguments in this case *)
      (* First argument can be on stack, second must be in register *)
      if stackp arg.(1)
      then [| arg.(0); self#makereg arg.(1) |]
      else arg
  | Ifloattest((Ceq|Cne|Cgt|Cge), _) ->
      (* Second argument can be on stack, first must be in register *)
      if stackp arg.(0)
      then [| self#makereg arg.(0); arg.(1) |]
      else arg
  | _ ->
      (* The argument(s) can be either in register or on stack *)
      arg

end

let fundecl f =
  (new reload)#fundecl f
