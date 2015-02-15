let a = get_some_value () in
  assert (a = 42); (* throws Assert_failure when a is not 42 *)
  (* evaluate stuff to return here when a is 42 *)
