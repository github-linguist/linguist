(module
  (import "env" "printInt" (func $printInt (param i32)))
  (import "env" "print" (func $print (param i32 i32)))

  (memory $memory 1)
  (data (i32.const 0) "\n")
  (data (i32.const 1) " ")

  (func $endl
    (call $print (i32.const 0) (i32.const 1)))
  (func $space
    (call $print (i32.const 1) (i32.const 1)))

  (func $fibonacci_rec (param $a i32) (param $b i32) (param $n i32) (result i32)
    (if (i32.eqz (get_local $n)) (return (get_local $a)))
    (call $printInt (get_local $b))
    (call $space)
    (set_local $a (i32.add (get_local $a) (get_local $b)))
    (call $fibonacci_rec (get_local $b) (get_local $a) (i32.sub (get_local $n) (i32.const 1)))
  )

  (func $fibonacci_iter (param $a i32) (param $b i32) (param $n i32) (result i32)
    (loop $fi
      (if (i32.eqz (get_local $n)) (return (get_local $a)))
      (call $printInt (get_local $b))
      (call $space)
      (set_local $b (i32.add (get_local $a) (get_local $b)))
      (set_local $a (i32.sub (get_local $b) (get_local $a)))
      (set_local $n (i32.sub (get_local $n) (i32.const 1)))
      (br $fi))
    (get_local $b))

  (func $main
    (drop (call $fibonacci_rec (i32.const 0) (i32.const 1) (i32.const 9)))
    (call $endl)
    (drop (call $fibonacci_iter (i32.const 0) (i32.const 1) (i32.const 9))))

  (export "main" (func $main))
  (export "memory" (memory $memory))
)

