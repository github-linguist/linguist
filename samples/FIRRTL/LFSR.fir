FIRRTL version 4.0.0
circuit Foo : %[[
  {
    "class": "firrtl.transforms.DontTouchAnnotation",
    "target": "~Foo|Foo>a"
  }
]]
  layer A, bind:
  module MaxPeriodFibonacciLFSR : @[src/main/scala/chisel3/util/random/FibonacciLFSR.scala 65:7]
    input clock : Clock @[src/main/scala/chisel3/util/random/FibonacciLFSR.scala 65:7]
    input reset : Reset @[src/main/scala/chisel3/util/random/FibonacciLFSR.scala 65:7]
    output io : { flip seed : { valid : UInt<1>, bits : UInt<1>[4]}, flip increment : UInt<1>, out : UInt<1>[4]} @[src/main/scala/chisel3/util/random/PRNG.scala 42:22]

    wire _state_WIRE : UInt<1>[4] @[src/main/scala/chisel3/util/random/PRNG.scala 46:28]
    connect _state_WIRE[0], UInt<1>(0h1) @[src/main/scala/chisel3/util/random/PRNG.scala 46:28]
    connect _state_WIRE[1], UInt<1>(0h0) @[src/main/scala/chisel3/util/random/PRNG.scala 46:28]
    connect _state_WIRE[2], UInt<1>(0h0) @[src/main/scala/chisel3/util/random/PRNG.scala 46:28]
    connect _state_WIRE[3], UInt<1>(0h0) @[src/main/scala/chisel3/util/random/PRNG.scala 46:28]
    regreset state : UInt<1>[4], clock, reset, _state_WIRE @[src/main/scala/chisel3/util/random/PRNG.scala 55:49]
    when io.increment : @[src/main/scala/chisel3/util/random/PRNG.scala 69:22]
      node _T = xor(state[3], state[2]) @[src/main/scala/chisel3/util/random/LFSR.scala 15:41]
      connect state[0], _T @[src/main/scala/chisel3/util/random/PRNG.scala 70:11]
      connect state[1], state[0] @[src/main/scala/chisel3/util/random/PRNG.scala 70:11]
      connect state[2], state[1] @[src/main/scala/chisel3/util/random/PRNG.scala 70:11]
      connect state[3], state[2] @[src/main/scala/chisel3/util/random/PRNG.scala 70:11]
    when io.seed.valid : @[src/main/scala/chisel3/util/random/PRNG.scala 73:22]
      connect state[0], io.seed.bits[0] @[src/main/scala/chisel3/util/random/PRNG.scala 74:11]
      connect state[1], io.seed.bits[1] @[src/main/scala/chisel3/util/random/PRNG.scala 74:11]
      connect state[2], io.seed.bits[2] @[src/main/scala/chisel3/util/random/PRNG.scala 74:11]
      connect state[3], io.seed.bits[3] @[src/main/scala/chisel3/util/random/PRNG.scala 74:11]
    connect io.out, state @[src/main/scala/chisel3/util/random/PRNG.scala 78:10]

  public module Foo : @[Users/user/repos/github.com/seldridge/scala-snippets/LFSR.scala 11:7]
    input clock : Clock @[Users/user/repos/github.com/seldridge/scala-snippets/LFSR.scala 11:7]
    input reset : UInt<1> @[Users/user/repos/github.com/seldridge/scala-snippets/LFSR.scala 11:7]
    output a : UInt<4> @[Users/user/repos/github.com/seldridge/scala-snippets/LFSR.scala 12:13]

    inst a_prng of MaxPeriodFibonacciLFSR @[src/main/scala/chisel3/util/random/PRNG.scala 91:22]
    connect a_prng.clock, clock
    connect a_prng.reset, reset
    connect a_prng.io.seed.valid, UInt<1>(0h0) @[src/main/scala/chisel3/util/random/PRNG.scala 92:24]
    invalidate a_prng.io.seed.bits[0] @[src/main/scala/chisel3/util/random/PRNG.scala 93:23]
    invalidate a_prng.io.seed.bits[1] @[src/main/scala/chisel3/util/random/PRNG.scala 93:23]
    invalidate a_prng.io.seed.bits[2] @[src/main/scala/chisel3/util/random/PRNG.scala 93:23]
    invalidate a_prng.io.seed.bits[3] @[src/main/scala/chisel3/util/random/PRNG.scala 93:23]
    connect a_prng.io.increment, UInt<1>(0h1) @[src/main/scala/chisel3/util/random/PRNG.scala 94:23]
    node a_lo = cat(a_prng.io.out[1], a_prng.io.out[0]) @[src/main/scala/chisel3/util/random/PRNG.scala 95:17]
    node a_hi = cat(a_prng.io.out[3], a_prng.io.out[2]) @[src/main/scala/chisel3/util/random/PRNG.scala 95:17]
    node _a_T = cat(a_hi, a_lo) @[src/main/scala/chisel3/util/random/PRNG.scala 95:17]
    connect a, _a_T @[Users/user/repos/github.com/seldridge/scala-snippets/LFSR.scala 14:5]
    printf(clock, UInt<1>(1), "Value of a is %x", a)
