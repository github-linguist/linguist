class FirstClassFns
{
  static |Obj -> Obj| compose (|Obj -> Obj| fn1, |Obj -> Obj| fn2)
  {
    return |Obj x -> Obj| { fn2 (fn1 (x)) }
  }

  public static Void main ()
  {
    cube := |Float a -> Float| { a * a * a }
    cbrt := |Float a -> Float| { a.pow(1/3f) }

    |Float->Float|[] fns := [Float#sin.func, Float#cos.func, cube]
    |Float->Float|[] inv := [Float#asin.func, Float#acos.func, cbrt]
    |Float->Float|[] composed := fns.map |fn, i| { compose(fn, inv[i]) }

    composed.each |fn| { echo (fn(0.5f)) }
  }
}
