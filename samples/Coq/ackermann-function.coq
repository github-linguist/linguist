Require Import Arith.
Fixpoint A m := fix A_m n :=
  match m with
    | 0 => n + 1
    | S pm =>
      match n with
        | 0 => A pm 1
        | S pn => A pm (A_m pn)
      end
  end.
