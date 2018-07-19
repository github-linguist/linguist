Section FOR.
  Variable T : Type.
  Variable body : nat -> T -> T.
  Variable start : nat.

  Fixpoint for_loop n : T -> T :=
    match n with
    | O => fun s => s
    | S n' => fun s => for_loop n' (body (start + n') s)
    end.

End FOR.

Eval vm_compute in
  for_loop _
    (fun i =>
      cons
        (for_loop _
          (fun j => cons tt)
          0 (S i) nil
        )
    )
    0 5 nil.
