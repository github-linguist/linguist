(* a coefficient function creates double values from in paramters *)
typedef coeff_f = int -> double

(* a continued fraction is described by a record of two coefficent functions a and b *)
typedef frac = @{a= coeff_f, b= coeff_f}

(* recursive definition of the approximation *)
fun calc_rec(f: frac, n: int, r: double): double =
  if n = 0 then
    f.a(0) + r // base case
  else let // recursive case
    val a: double = f.a(n) // record access
    val b: double = f.b(n)
    val s: double = b / (a + r)
    //val () = printf("r%d = %f, a = %f, b = %f, r%d = %f\n", @(n, r, a, b, (n-1), s))
  in calc_rec(f, n - 1, s) end

fun calc(f: frac, n: int): double =
  calc_rec(f, n, 0.0)

(* anonymous coefficient functions created with lam (short for lambda) *)
val sqrt2 = @{ a= lam (n: int): double => if n = 0 then 1.0 else 2.0,
               b= lam (n: int): double => 1.0 }

val napier = @{ a= lam (n: int): double => if n = 0 then 2.0 else 1.0 * (n),
                b= lam (n: int): double => if n = 1 then 1.0 else n - 1.0 }

fun square(x: double): double = x * x

val pi = @{ a= lam (n: int): double => if n = 0 then 3.0 else 6.0,
            b= lam (n: int): double => square (2.0 * n - 1) }

implement main () = begin
  printf ("sqrt2  = %f\n", @(calc(sqrt2,  100)));
  printf ("napier = %f\n", @(calc(napier, 100)));
  printf ("  pi   = %f\n", @(calc(  pi  , 100)));
end
