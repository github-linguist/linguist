open Format;

let module Endo = {
  type t 'a = 'a => 'a;
};

let module Syntax = {
  let module Var = {
    type t = int;
  };
  let module Term = {
    type t =
      | App t t
      | Lam t
      | Var Var.t
      ;
  };
  let module Sub = {
    type t 'a =
      | Cmp (t 'a) (t 'a)
      | Dot 'a (t 'a)
      | Id
      | Shift
      ;

    let map f sgm => {
      let rec go = fun
      | Cmp sgm0 sgm1 => Cmp (go sgm0) (go sgm1)
      | Dot a sgm => Dot (f a) (go sgm)
      | Id => Id
      | Shift => Shift
      ;
      go sgm;
    };

    let rec apply sgm e =>
      switch (sgm, e) {
      | (sgm, Term.App e0 e1) => Term.App (apply sgm e0) (apply sgm e1)
      | (sgm, Term.Lam e) => Term.Lam (apply (Dot (Term.Var 0) (Cmp sgm Shift)) e)
      | (Dot e _, Term.Var 0) => e
      | (Dot _ sgm, Term.Var i) => apply sgm (Term.Var (i - 1))
      | (Id, Term.Var i) => Term.Var i
      | (Shift, Term.Var i) => Term.Var (i + 1)
      | (Cmp rho sgm, e) => apply sgm (apply rho e)
      };
  };
};

let module Zip = {
  open Syntax;
  type t 'a =
    | App0 (t 'a) 'a
    | App1 'a (t 'a)
    | Halt
    | Lam (t 'a)
    ;

  let map f sgm => {
    let rec go = fun
    | App0 zip e1 => App0 (go zip) (f e1)
    | App1 e0 zip => App1 (f e0) (go zip)
    | Halt => Halt
    | Lam zip => Lam (go zip)
    ;
    go sgm;
  };

  let rec apply zip acc => switch zip {
    | App0 zip e1 => apply zip (Term.App acc e1)
    | App1 e0 zip => apply zip (Term.App e0 acc)
    | Halt => acc
    | Lam zip => apply zip (Term.Lam acc)
  };
};

let module Clo = {
  open Syntax;
  type t =
    | Clo Term.t (Sub.t t);
  let rec from (Clo term sgm) => Sub.apply (Sub.map from sgm) term;
};

let module Pretty = {
  let module Delim = {
    type t = string;
    let pp prev next fmt token => if (prev < next) { fprintf fmt "%s" token };
  };
  let module Prec = {
    type t = int;
    open Syntax.Term;
    let calc = fun
      | App _ _ => 1
      | Lam _ => 2
      | Var _ => 0
      ;
  };
  let module Name = {
    type t = string;

    let suffix = {
      let script = fun
        | 0 => "₀"
        | 1 => "₁"
        | 2 => "₂"
        | 3 => "₃"
        | 4 => "₄"
        | 5 => "₅"
        | 6 => "₆"
        | 7 => "₇"
        | 8 => "₈"
        | 9 => "₉"
        | _ => failwith "bad subscript";
      let rec go acc => fun
        | 0 => acc
        | n => go (script (n mod 10) ^ acc) (n / 10);
      go ""
    };

    let gen = {
      let offset = 97;
      let width = 26;
      fun () i => {
        let code = i mod width + offset;
        let char = Char.chr code;
        let prime = i / width;
        let suffix = suffix prime;
        let name = Char.escaped char ^ suffix;
        Some name;
      }
    };
  };

  let module Env = {
    type t = {
      used: list Name.t,
      rest: Stream.t Name.t,
    };
    let mk () => {
      let used = [];
      let rest = Stream.from @@ Name.gen ();
      { used, rest };
    };
  };

  type printer 'a = Env.t => Prec.t => formatter => 'a => unit;

  let module Term = {
    open Syntax.Term;
    let rec pp ({ Env.used: used, rest } as env) prev fmt e => {
      let next = Prec.calc e;
      switch e {
      | App e0 e1 =>
        fprintf fmt "@[%a%a@ %a%a@]"
          (Delim.pp prev next) "("
          (pp env 1) e0
          (pp env 0) e1
          (Delim.pp prev next) ")"
      | Lam e =>
        let name = Stream.next rest;
        let env = { ...env, Env.used: [name, ...used] };
        fprintf fmt "%aλ%a.%a%a"
          (Delim.pp prev next) "("
          (pp_print_string) name
          (pp env next) e
          (Delim.pp prev next) ")"
      | Var index =>
        fprintf fmt "%s" @@ try (List.nth used index) {
          | _ => "#" ^ string_of_int index
          }
      }
    };
  };

  let module Sub = {
    open Syntax.Sub;
    let rec pp pp_elem env prev fmt => fun
    | Cmp sgm1 sgm0 =>
      fprintf fmt "@[%a;@ %a@]"
        (pp pp_elem env prev) sgm1
        (pp pp_elem env prev) sgm0
    | Dot e sgm =>
      fprintf fmt "@[%a@ ·@ %a@]"
        (pp_elem env prev) e
        (pp pp_elem env prev) sgm
    | Id =>
      fprintf fmt "ι"
    | Shift =>
      fprintf fmt "↑"
    ;
  };

  let module Clo = {
    let rec pp env prev fmt (Clo.Clo e sgm) => {
      let next = Prec.calc e;
      fprintf fmt "@[%a%a%a[%a]@]"
        (Delim.pp prev next) "("
        (Term.pp env next) e
        (Delim.pp prev next) ")"
        (Sub.pp pp env next) sgm
    };
  };

  let module Zip = {
    open Zip;
    let rec pp pp_elem env prev fmt => fun
    | App0 zip elem =>
      fprintf fmt "inl@[<v -1>⟨@,%a@,%a⟩@]"
        (pp pp_elem env prev) zip
        (pp_elem env prev) elem
    | App1 elem zip =>
      fprintf fmt "inr@[<v -1>⟨@,%a@,%a⟩@]"
        (pp_elem env prev) elem
        (pp pp_elem env prev) zip
    | Halt =>
      fprintf fmt "halt"
    | Lam zip =>
      fprintf fmt "lam@[<v -1>⟨@,%a⟩@]"
        (pp pp_elem env prev) zip
    ;
  };
};

let module Machine = {
  type t = {
    clo: Clo.t,
    ctx: Zip.t Clo.t,
  };

  let into e => {
    open Clo;
    open Syntax.Sub;
    let clo = Clo e Id;
    let ctx = Zip.Halt;
    { clo, ctx }
  };

  let from { clo, ctx } => Zip.apply (Zip.map Clo.from ctx) (Clo.from clo);

  let pp fmt rule state => {
    fprintf fmt "@[<v>ctx  ::@[<v -5>@,%a@]@,clo  ::@[<v -5>@,%a@]@,rule ::@[<v -5>@,%a@]@,term ::@[<v -5>@,%a@]@]@."
      (Pretty.Zip.pp Pretty.Clo.pp (Pretty.Env.mk ()) 2) state.ctx
                    (Pretty.Clo.pp (Pretty.Env.mk ()) 2) state.clo
                                       (pp_print_string) rule
                   (Pretty.Term.pp (Pretty.Env.mk ()) 2) (from state)
  };

  let halted state => {
    open Clo;
    open Syntax.Sub;
    open Syntax.Term;
    switch state {
    | { clo: Clo (Var _) Id, _ } => true
    | _ => false
    } [@warning "-4"];
  };

  let step state => {
    open Clo;
    open Syntax.Sub;
    open Syntax.Term;
    let rule = ref "";
    let state = switch state {
    /* left */
    | { clo: Clo (App e0 e1) sgm, ctx } =>
      let clo = Clo e0 sgm;
      let ctx = Zip.App0 ctx (Clo e1 sgm);
      rule := "LEFT";
      { clo, ctx };
    /* beta */
    | { clo: Clo (Lam e) sgm, ctx: Zip.App0 ctx c0 } =>
      let clo = Clo e (Cmp (Dot c0 sgm) Id);
      rule := "BETA";
      { clo, ctx };
    /* lambda */
    | { clo: Clo (Lam e) sgm, ctx } =>
      let clo = Clo e (Cmp (Dot (Clo (Var 0) Id) (Cmp sgm Shift)) Id);
      let ctx = Zip.Lam ctx;
      rule := "LAMBDA";
      { clo, ctx };
    /* associate */
    | { clo: Clo (Var n) (Cmp (Cmp pi rho) sgm), ctx } =>
      let clo = Clo (Var n) (Cmp pi (Cmp rho sgm));
      rule := "ASSOCIATE";
      { clo, ctx };
    /* head */
    | { clo: Clo (Var 0) (Cmp (Dot (Clo e pi) _) sgm), ctx } =>
      let clo = Clo e (Cmp pi sgm);
      rule := "HEAD";
      { clo, ctx };
    /* tail */
    | { clo: Clo (Var n) (Cmp (Dot (Clo _ _) rho) sgm), ctx } =>
      let clo = Clo (Var (n - 1)) (Cmp rho sgm);
      rule := "TAIL";
      { clo, ctx };
    /* shift */
    | { clo: Clo (Var n) (Cmp Shift sgm), ctx } =>
      let clo = Clo (Var (n + 1)) sgm;
      rule := "SHIFT";
      { clo, ctx };
    /* id */
    | { clo: Clo (Var n) (Cmp Id sgm), ctx } =>
      let clo = Clo (Var n) sgm;
      rule := "ID";
      { clo, ctx };
    | _ =>
      pp std_formatter !rule state;
      failwith "bad state";
    } [@warning "-4"];
    pp std_formatter !rule state;
    state;
  };

  let norm e => {
    let count = ref 0;
    let state = ref (into e);
    while (not (halted !state)) {
      fprintf std_formatter "@\n--- step[%d] ---@\n" !count;
      incr count;
      state := step !state;
    };
    from !state;
  };
};

let module Test = {
  open Syntax.Term;
  let l e => Lam e;
  let ( *@ ) e0 e1 => App e0 e1;
  let ff = l (l (Var 1));
  let tt = l (l (Var 0));
  let zero = l (l (Var 1));
  let succ = l (l (l (Var 0 *@ Var 2)));
  let one = succ *@ zero;
  let two = succ *@ one;
  let three = succ *@ two;
  let const = l (l (Var 1));
  let fix = l (l (Var 1 *@ (Var 0 *@ Var 0)) *@ l (Var 1 *@ (Var 0 *@ Var 0)));
  let add = fix *@ l (l (l (Var 1 *@ Var 0 *@ l (succ *@ Var 3 *@ Var 0 *@ Var 1))));
  let init = l (l (Var 0) *@ l (l (Var 1)));
};

let module Run = {
  let go () => Machine.norm Test.init;
};