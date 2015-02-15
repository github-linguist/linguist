/* .<n>. is a termination metric to prove that the function terminates. It can be omitted. */
fun bottles {n:nat} .<n>. (n: int n): void =
    if n = 0 then
        ()
    else begin
        printf ("%d bottles of beer on the wall\n", @(n));
        printf ("%d bottles of beer\n", @(n));
        printf ("Take one down, pass it around\n", @());
        printf ("%d bottles of beer on the wall\n", @(n-1));
        bottles (n - 1)
    end

implement main () = bottles (99)
