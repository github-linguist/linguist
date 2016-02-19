use "options"
use "time"
use "collections"

class Config
  var logtable: U64 = 20
  var iterate: U64 = 10000
  var logchunk: U64 = 10
  var logactors: U64 = 2

  fun ref apply(env: Env): Bool =>
    var options = Options(env)

    options
      .add("logtable", "l", I64Argument)
      .add("iterate", "i", I64Argument)
      .add("chunk", "c", I64Argument)
      .add("actors", "a", I64Argument)

    for option in options do
      match option
      | ("table", var arg: I64) => logtable = arg.u64()
      | ("iterate", var arg: I64) => iterate = arg.u64()
      | ("chunk", var arg: I64) => logchunk = arg.u64()
      | ("actors", var arg: I64) => logactors = arg.u64()
      | let err: ParseError =>
        err.report(env.out)
        env.out.print(
          """
          gups_opt [OPTIONS]
            --table     N   log2 of the total table size. Defaults to 20.
            --iterate   N   number of iterations. Defaults to 10000.
            --chunk     N   log2 of the chunk size. Defaults to 10.
            --actors    N   log2 of the actor count. Defaults to 2.
          """
          )
        return false
      end
    end

    env.out.print(
      "logtable: " + logtable.string() +
      "\niterate: " + iterate.string() +
      "\nlogchunk: " + logchunk.string() +
      "\nlogactors: " + logactors.string()
      )
    true

actor Main
  let _env: Env
  let _config: Config = Config

  var _updates: U64 = 0
  var _confirm: U64 = 0
  let _start: U64
  var _actors: Array[Updater] val

  new create(env: Env) =>
    _env = env

    if _config(env) then
      let actor_count = 1 << _config.logactors
      let loglocal = _config.logtable - _config.logactors
      let chunk_size = 1 << _config.logchunk
      let chunk_iterate = chunk_size * _config.iterate

      _updates = chunk_iterate * actor_count
      _confirm = actor_count

      var updaters = recover Array[Updater](actor_count) end

      for i in Range[U64](0, actor_count) do
        updaters.push(Updater(this, actor_count, i, loglocal, chunk_size,
          chunk_iterate * i))
      end

      _actors = consume updaters
      _start = Time.nanos()

      for a in _actors.values() do
        a.start(_actors, _config.iterate)
      end
    else
      _start = 0
      _actors = recover Array[Updater] end
    end

  be done() =>
    if (_confirm = _confirm - 1) == 1 then
      for a in _actors.values() do
        a.done()
      end
    end

  be confirm() =>
    _confirm = _confirm + 1

    if _confirm == _actors.size() then
      let elapsed = (Time.nanos() - _start).f64()
      let gups = _updates.f64() / elapsed

      _env.out.print(
        "Time: " + (elapsed / 1e9).string() +
        "\nGUPS: " + gups.string()
        )
    end

actor Updater
  let _main: Main
  let _index: U64
  let _updaters: U64
  let _chunk: U64
  let _mask: U64
  let _loglocal: U64

  let _output: Array[Array[U64] iso]
  let _reuse: List[Array[U64] iso] = List[Array[U64] iso]
  var _others: (Array[Updater] val | None) = None
  var _table: Array[U64]
  var _rand: U64

  new create(main:Main, updaters: U64, index: U64, loglocal: U64, chunk: U64,
    seed: U64)
  =>
    _main = main
    _index = index
    _updaters = updaters
    _chunk = chunk
    _mask = updaters - 1
    _loglocal = loglocal

    _rand = PolyRand.seed(seed)
    _output = _output.create(updaters)

    let size = 1 << loglocal
    _table = Array[U64].undefined(size)

    var offset = index * size

    try
      for i in Range[U64](0, size) do
        _table(i) = i + offset
      end
    end

  be start(others: Array[Updater] val, iterate: U64) =>
    _others = others
    iteration(iterate)

  be apply(iterate: U64) =>
    iteration(iterate)

  fun ref iteration(iterate: U64) =>
    let chk = _chunk

    for i in Range(0, _updaters) do
      _output.push(
        try
          _reuse.pop()
        else
          recover Array[U64](chk) end
        end
        )
    end

    for i in Range(0, _chunk) do
      var datum = _rand = PolyRand(_rand)
      var updater = (datum >> _loglocal) and _mask

      try
        if updater == _index then
          _table(i) = _table(i) xor datum
        else
          _output(updater).push(datum)
        end
      end
    end

    try
      let to = _others as Array[Updater] val

      repeat
        let data = _output.pop()

        if data.size() > 0 then
          to(_output.size()).receive(consume data)
        else
          _reuse.push(consume data)
        end
      until _output.size() == 0 end
    end

    if iterate > 1 then
      apply(iterate - 1)
    else
      _main.done()
    end

  be receive(data: Array[U64] iso) =>
    try
      for i in Range(0, data.size()) do
        let datum = data(i)
        var j = (datum >> _loglocal) and _mask
        _table(j) = _table(j) xor datum
      end

      data.clear()
      _reuse.push(consume data)
    end

  be done() =>
    _main.confirm()

primitive PolyRand
  fun apply(prev: U64): U64 =>
    (prev << 1) xor if prev.i64() < 0 then _poly() else 0 end

  fun seed(from: U64): U64 =>
    var n = from % _period()

    if n == 0 then
      return 1
    end

    var m2 = Array[U64].undefined(64)
    var temp = U64(1)

    try
      for i in Range(0, 64) do
        m2(i) = temp
        temp = this(temp)
        temp = this(temp)
      end
    end

    var i: U64 = 64 - n.clz()
    var r = U64(2)

    try
      while i > 0 do
        temp = 0

        for j in Range(0, 64) do
          if ((r >> j) and 1) != 0 then
            temp = temp xor m2(j)
          end
        end

        r = temp
        i = i - 1

        if ((n >> i) and 1) != 0 then
          r = this(r)
        end
      end
    end
    r

  fun _poly(): U64 => 7

  fun _period(): U64 => 1317624576693539401
