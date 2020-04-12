// This method computes the sum and max of a given array of
// integers.  The method's postcondition only promises that
// 'sum' will be no greater than 'max'.  Can you write a
// different method body that also achieves this postcondition?
// Hint: Your program does not have to compute the sum and
// max of the array, despite the suggestive names of the
// out-parameters.
method M(N: int, a: array<int>) returns (sum: int, max: int)
  requires 0 <= N && a != null && a.Length == N
  ensures sum <= N * max
{
  sum := 0;
  max := 0;
  var i := 0;
  while i < N
    invariant i <= N && sum <= i * max
  {
    if max < a[i] {
      max := a[i];
    }
    sum := sum + a[i];
    i := i + 1;
  }
}
