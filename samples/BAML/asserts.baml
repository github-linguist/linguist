
function AssertFn(a: int) -> int {
  client GPT35
  prompt #"
    Return the integer after {{ a }}

    {{ ctx.output_format }}
  "#
}


test AssertFn {
  functions [AssertFn]
  args {
    a 1
  }
  // test syntax highlighting
  @@assert(test_name, {{ this[0].datetime == "hi, thereerwc[]{}[]"}})
  @@assert({{"let me know if ,er"}})
  @@assert(test_name, {{ this[0].datetime == "Wed, 06 nov (PST)"}})
}
