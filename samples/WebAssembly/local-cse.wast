(module
  (memory 100 100)
  (func $basics
    (local $x i32)
    (local $y i32)
    (drop
      (i32.add (i32.const 1) (i32.const 2))
    )
    (drop
      (i32.add (i32.const 1) (i32.const 2))
    )
    (if (i32.const 0) (nop))
    (drop ;; we can't do this yet, non-linear
      (i32.add (i32.const 1) (i32.const 2))
    )
    (drop
      (i32.add (get_local $x) (get_local $y))
    )
    (drop
      (i32.add (get_local $x) (get_local $y))
    )
    (drop
      (i32.add (get_local $x) (get_local $y))
    )
    (call $basics) ;; side effects, but no matter for our locals
    (drop
      (i32.add (get_local $x) (get_local $y))
    )
    (set_local $x (i32.const 100))
    (drop ;; x was changed!
      (i32.add (get_local $x) (get_local $y))
    )
  )
  (func $recursive1
    (local $x i32)
    (local $y i32)
    (drop
      (i32.add
        (i32.const 1)
        (i32.add
          (i32.const 2)
          (i32.const 3)
        )
      )
    )
    (drop
      (i32.add
        (i32.const 1)
        (i32.add
          (i32.const 2)
          (i32.const 3)
        )
      )
    )
    (drop
      (i32.add
        (i32.const 2)
        (i32.const 3)
      )
    )
  )
  (func $recursive2
    (local $x i32)
    (local $y i32)
    (drop
      (i32.add
        (i32.const 1)
        (i32.add
          (i32.const 2)
          (i32.const 3)
        )
      )
    )
    (drop
      (i32.add
        (i32.const 2)
        (i32.const 3)
      )
    )
    (drop
      (i32.add
        (i32.const 1)
        (i32.add
          (i32.const 2)
          (i32.const 3)
        )
      )
    )
  )
  (func $self
    (local $x i32)
    (local $y i32)
    (drop
      (i32.add
        (i32.add
          (i32.const 2)
          (i32.const 3)
        )
        (i32.add
          (i32.const 2)
          (i32.const 3)
        )
      )
    )
    (drop
      (i32.add
        (i32.const 2)
        (i32.const 3)
      )
    )
  )
  (func $loads
    (drop
      (i32.load (i32.const 10))
    )
    (drop
      (i32.load (i32.const 10)) ;; implicit traps, sad
    )
  )
  (func $8 (param $var$0 i32) (result i32)
    (local $var$1 i32)
    (local $var$2 i32)
    (local $var$3 i32)
    (block $label$0 i32
      (i32.store
        (tee_local $var$2
          (i32.add
            (get_local $var$1)
            (i32.const 4)
          )
        )
        (i32.and
          (i32.load
            (get_local $var$2)
          )
          (i32.xor
            (tee_local $var$2
              (i32.const 74)
            )
            (i32.const -1)
          )
        )
      )
      (i32.store
        (tee_local $var$1
          (i32.add
            (get_local $var$1)
            (i32.const 4)
          )
        )
        (i32.or
          (i32.load
            (get_local $var$1)
          )
          (i32.and
            (get_local $var$2)
            (i32.const 8)
          )
        )
      )
      (i32.const 0)
    )
  )
)
