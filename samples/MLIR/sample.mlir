// CHECK-LABEL: func @func_with_ops(%arg0: f32) {
func @func_with_ops(%a : f32) {
  // CHECK: %0 = "getTensor"() : () -> tensor<4x4x?xf32>
  %t = "getTensor"() : () -> tensor<4x4x?xvector<10xf32>>>

  %i6 = muli %i2, %i2 : i32
  %t2 = "std.dim"(%t){index = 2} : (tensor<4x4x?xvector<10xf32>>) -> index
  %x = "foo"(%a, %a) : (f32,f32) -> (memref<1 x i32, (d0) -> (d0), 4>)

  return
}

func @count(%x: tensor<i64) -> (i64, i64)
  attributes {fruit = "banana"} {
  return %x, %x: i64, i64
}

func @correct_number_of_regions() {
    // CHECK: test.two_region_op
    "test.two_region_op"()(
      {"work"() : () -> ()},
      {"work"() : () -> ()}
    ) : () -> ()
    return
}

func @inline_notation() -> i32 {
  %1 = "foo"() : () -> i32 loc("foo")
  %1p = "foo"() : () -> i32 loc(fused<"myPass">["abc", "de"])

  // CHECK: constant 4 : index loc(callsite("foo" at "mysource.cc":10:8))
  %2 = constant 4 : index loc(callsite("foo" at "mysource.cc":10:8))

  affine.for %i0 = 0 to 8 {
  } loc(fused["foo", "mysource.cc":10:8])

  affine.if #set0(%2) {
  } loc(fused<"myPass">["foo", "foo2"])

  return %1 : i32 loc(unknown)
}

func @simple(i64, i1) -> i64 {
^bb0(%a: i64, %cond: i1): // Code dominated by ^bb0 may refer to %a
  cond_br %cond, ^bb1, ^bb2

^bb1:
  br ^bb3(%a: i64)    // Branch passes %a as the argument

^bb2:
  %b = addi %a, %a : i64
  br ^bb3(%b: i64)    // Branch passes %b as the argument

// ^bb3 receives an argument, named %c, from predecessors
// and passes it on to bb4 twice.
^bb3(%c: i64):
  br ^bb4(%c, %c : i64, i64)

^bb4(%d : i64, %e : i64):
  %0 = addi %d, %e : i64
  return %0 : i64
}

// CHECK-LABEL: func @func_with_ops(%arg0: f32) {
func @func_with_ops(f32) {
^bb0(%a : f32):
  %t = "getTensor"() : () -> tensor<4x4x?xf32>
  %t2 = "std.dim"(%t){index = 2} : (tensor<4x4x?xf32>) -> index

  %x = "std.addf"(%a, %a) : (f32,f32) -> (f32) // help

  return
}

func @multiblock() {
  return     // CHECK:   return
^bb1:         // CHECK: ^bb1:   // no predecessors
  br ^bb4     // CHECK:   br ^bb3
^bb2:         // CHECK: ^bb2:   // pred: ^bb2
  br ^bb2     // CHECK:   br ^bb2
^bb4:         // CHECK: ^bb3:   // pred: ^bb1
  return     // CHECK:   return
}

func @dialect_attribute_with_type() {
  "foo.unknown_op"() {foo = #foo.attr : i32} : () -> ()
}

func @inline_notation() -> i32 {
  %1 = "foo"() : () -> i32 loc("foo")

  %2 = constant 4 : index loc(callsite("foo" at "mysource.cc":10:8))

  affine.for %i0 = 0 to 8 {
  } loc(fused["foo", "mysource.cc":10:8])

  affine.if #set0(%2) {
  } loc(fused<"myPass">["foo", "foo2"])

  return %1 : i32 loc(unknown)
}

