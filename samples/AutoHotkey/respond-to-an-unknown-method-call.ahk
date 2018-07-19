class example
{
  foo()
  {
    Msgbox Called example.foo()
  }

  __Call(method, params*)
  {
    funcRef := Func(funcName := this.__class "." method)
    if !IsObject(funcRef)
    {
      str := "Called undefined method " funcName "() with these parameters:"
      for k,v in params
        str .= "`n" v
      Msgbox %str%
    }
    else
    {
      return funcRef.(this, params*)
    }
  }
}

ex := new example
ex.foo()
ex.bar(1,2)
