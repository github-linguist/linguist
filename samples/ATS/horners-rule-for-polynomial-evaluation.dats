staload "prelude/SATS/list.sats"
staload "prelude/DATS/list.dats"

fun horner (x: int, coeff: List int): int = let
  val f = lam (a: int, b: int) =<cloref1> a + b*x
in
  list_fold_right_cloref (f, coeff, 0)
end

implement main () = let
  val x = 3
  val coeff = '[~19,7,~4,6]
  val res = horner (x, coeff)
in
  print! (res, "\n")
end
