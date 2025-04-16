(module
  (memory 256 256)
  (type $0 (func (param i32)))
  (type $1 (func))
  (type $2 (func (result i32)))
  (func $b14 (type $2)
    (drop
      (if i32 ;; with shrinking, this can become a select
        (i32.const 1)
        (block $block1 i32
          (i32.const 12)
        )
        (block $block3 i32
          (i32.const 27)
        )
      )
    )
    (drop
      (if i32
        (i32.const 1)
        (i32.load (i32.const 10)) ;; load may have side effects, unless ignored
        (i32.const 27)
      )
    )
    (drop
      (if i32
        (i32.const 1)
        (i32.rem_s (i32.const 11) (i32.const 12)) ;; rem may have side effects, unless ignored
        (i32.const 27)
      )
    )
    (drop
      (if i32
        (i32.const 1)
        (i32.trunc_u/f64 (f64.const 12.34)) ;; float to int may have side effects, unless ignored
        (i32.const 27)
      )
    )
    (i32.const 0)
  )
  (func $join-br_ifs
    (block $out
      (br_if $out (i32.const 1))
      (br_if $out (i32.const 2))
      (br_if $out (i32.const 3))
    )
    (block $out2
      (block $out3
        (br_if $out2 (i32.const 1))
        (br_if $out3 (i32.const 2))
        (br_if $out2 (i32.const 3))
      )
      (unreachable)
    )
    (block $out4
      (block $out5
        (br_if $out4 (i32.const 1))
        (br_if $out5 (i32.const 2))
        (br_if $out5 (i32.const 3))
      )
      (unreachable)
    )
    (block $out6
      (block $out7
        (br_if $out6 (i32.const 1))
        (br_if $out6 (i32.const 2))
        (br_if $out7 (i32.const 3))
      )
      (unreachable)
    )
    (block $out8
      (br_if $out8 (call $b14)) ;; side effect
      (br_if $out8 (i32.const 0))
    )
    (block $out8
      (br_if $out8 (i32.const 1))
      (br_if $out8 (call $b14)) ;; side effect
    )
  )
)

