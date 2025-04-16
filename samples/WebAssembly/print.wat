(module
  (import "env" "printInt" (func $printInt (param i32)))
  (import "env" "printFloat" (func $printFloat (param f32)))
  (import "env" "print" (func $print (param i32 i32)))

  (memory $memory 1)
  (data (i32.const 0) "\n")
  (data (i32.const 1) "Hello World!")

  (func $endl
    (call $print (i32.const 0) (i32.const 1)))

  (func $main
    (call $printInt (i32.const 9))
    (call $endl)
    (call $printFloat (f32.const 6.28))
    (call $endl)
    (call $print (i32.const 1) (i32.const 12))
    )

  (export "main" (func $main))
  (export "memory" (memory $memory))
)
