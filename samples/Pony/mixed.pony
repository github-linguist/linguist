use "collections"

actor Worker
  var _env: Env

  new create(env: Env) =>
    _env = env

    var a: U64 = 86028157
    var b: U64 = 329545133

    var result = factorize(a*b)

    var correct =
      try
        (result.size() == 2) and
          (result(0) == 86028157) and
          (result(1) == 329545133)
      else
        false
      end

  fun ref factorize(bigint: U64) : Array[U64] =>
    var factors = Array[U64](2)

    if bigint <= 3 then
      factors.push(bigint)
    else
      var d: U64 = 2
      var i: U64 = 0
      var n = bigint

      while d < n do
        if (n % d) == 0 then
          i = i + 1
          factors.push(d)
          n = n / d
        else
          d = if d == 2 then 3 else (d + 2) end
        end
      end

      factors.push(d)
    end

    factors

actor Ring
  var _env: Env
  var _size: U32
  var _pass: U32
  var _repetitions: U32
  var _next: Ring

  new create(env: Env, size: U32, pass: U32, repetitions: U32) =>
    _env = env
    _size = size
    _pass = pass
    _repetitions = repetitions
    _next = spawn_ring(_env, _size, _pass)
    run()

  new neighbor(env: Env, next: Ring) =>
    _env = env
    _next = next
    _size = 0
    _pass = 0
    _repetitions = 0

  be apply(i: U32) =>
    if i > 0 then
      _next(i - 1)
    else
      run()
    end

  fun ref run() =>
    if _repetitions > 0 then
      _repetitions = _repetitions - 1
      _next(_pass * _size)
      Worker(_env)
    end

  fun tag spawn_ring(env: Env, size: U32, pass': U32) : Ring =>
    var next: Ring = this

    for i in Range[U32](0, size) do
      next = Ring.neighbor(env, next)
    end

    next

actor Main
  var _size: U32 = 50
  var _count: U32 = 20
  var _pass: U32 = 10000
  var _repetitions: U32 = 5
  var _env: Env

  new create(env: Env) =>
    _env = env

    try
      arguments()
      start_benchmark()
    else
      usage()
    end

  fun ref arguments() ? =>
    _count = _env.args(1).u32()
    _size = _env.args(2).u32()
    _pass = _env.args(3).u32()
    _repetitions = _env.args(4).u32()
    
  fun ref start_benchmark() =>
    for i in Range[U32](0, _count) do
      Ring(_env, _size, _pass, _repetitions)
    end

  fun ref usage() =>
    _env.out.print(
      """
      mixed OPTIONS
        N   number of actors in each ring"
        N   number of rings"
        N   number of messages to pass around each ring"
        N   number of times to repeat"
      """
      )
