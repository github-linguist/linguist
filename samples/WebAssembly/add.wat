(module
  (import "env" "printInt" (func $printInt (param i32)))
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    get_local $lhs
    get_local $rhs
    i32.add
  )

  (func $main
    (call $printInt
      (call $add (i32.const 9) (i32.const 8))))

  (export "main" (func $main))
)
