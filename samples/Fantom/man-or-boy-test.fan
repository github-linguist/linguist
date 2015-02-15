class ManOrBoy
{
  Void main()
  {
    echo(A(10, |->Int|{1}, |->Int|{-1}, |->Int|{-1}, |->Int|{1}, |->Int|{0}));
  }

  static Int A(Int k, |->Int| x1, |->Int| x2, |->Int| x3, |->Int| x4, |->Int| x5)
  {
    |->Int|? b
    b = |->Int| { k--; return A(k, b, x1, x2, x3, x4) }
    return k <= 0 ? x4() + x5() : b()
  }
}
