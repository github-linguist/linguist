%dw 2.0
var number = 1234
fun foo(func,name="Mariano") = func(name)
input payload application/test arg="value"
output application/json
---
{
  foo: "bar"
}