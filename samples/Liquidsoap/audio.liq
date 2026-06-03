# Multiband compression. The list in argument specifies
# - the `frequency` below which we should apply compression (it is above previous band)
# - the `attack` time (ms)
# - the `release` time (ms)
# - the compression `ratio`
# - the `threshold` for compression
# - the `gain` for the band
# @category Source / Audio processing
# @param ~limit Also apply limiting to bands.
# @param l Parameters for compression bands.
# @param s Source on which multiband compression should be applied.
# @flag extra
def compress.multiband(~limit=true, ~wet=getter(1.), s, l) =
  # Check that the bands are with increasing frequencies.
  for
  i
  =
  0
  to
  list.length(l) - 2
  do
    if
      getter.get(list.nth(l, i + 1).frequency) <
        getter.get(list.nth(l, i).frequency)
    then
      failwith("Bands should be sorted.")
    end
  end

  # Process a band
  def band(low, band) =
    high =
      if
        getter.is_constant(band.frequency)
      and
        getter.get(band.frequency) >= float_of_int(audio.samplerate()) / 2.
      then
        infinity
      else
        band.frequency
      end

    s = filter.iir.eq.low_high(low=low, high=high, s)
    s =
      compress(
        attack=band.attack,
        release=band.release,
        threshold=band.threshold,
        ratio=band.ratio,
        gain=band.gain,
        s
      )

    if limit then limiter(s) else s end
  end

  ls =
    list.mapi(
      fun (i, b) ->
        band(if i == 0 then 0. else list.nth(l, i - 1).frequency end, b),
      l
    )

  c = add(normalize=false, ls)
  s =
    if
      not getter.is_constant(wet) or getter.get(wet) != 1.
    then
      add(
        normalize=false, [amplify({1. - getter.get(wet)}, s), amplify(wet, c)]
      )
    else
      c
    end


  # Seal l element type
  if false then () else list.hd(l) end

  # Limit to avoid bad surprises
  limiter(s)
end

# Compress and normalize, producing a more uniform and "full" sound.
# @category Source / Audio processing
# @flag extra
# @param s The input source.
def nrj(s) =
  compress(threshold=-15., ratio=3., gain=3., normalize(s))
end

# Multiband-compression.
# @category Source / Audio processing
# @flag extra
# @param s The input source.
def sky(s) =
  # 3-band crossover
  low = fun (s) -> filter.iir.eq.low(frequency=168., s)
  mh = fun (s) -> filter.iir.eq.high(frequency=100., s)
  mid = fun (s) -> filter.iir.eq.low(frequency=1800., s)
  high = fun (s) -> filter.iir.eq.high(frequency=1366., s)

  # Add back
  add(
    normalize=false,
    [
      compress(
        attack=100.,
        release=200.,
        threshold=-20.,
        ratio=6.,
        gain=6.7,
        knee=0.3,
        low(s)
      ),
      compress(
        attack=100.,
        release=200.,
        threshold=-20.,
        ratio=6.,
        gain=6.7,
        knee=0.3,
        mid(mh(s))
      ),
      compress(
        attack=100.,
        release=200.,
        threshold=-20.,
        ratio=6.,
        gain=6.7,
        knee=0.3,
        high(s)
      )
    ]
  )
end

# Generate DTMF tones.
# @flag extra
# @category Source / Sound synthesis
# @param ~duration Duration of a tone (in seconds).
# @param ~delay Dealy between two successive tones (in seconds).
# @param dtmf String describing DTMF tones to generates: it should contains characters 0 to 9, A to D, or * or #.
def replaces dtmf(~duration=0.1, ~delay=0.05, dtmf) =
  l = ref([])
  for
  i
  =
  0
  to
  string.length(dtmf) - 1
  do
    c = string.sub(dtmf, start=i, length=1)
    let (row, col) =
      if
        c == "1"
      then
        (697., 1209.)
      elsif c == "2" then (697., 1336.)
      elsif c == "3" then (697., 1477.)
      elsif c == "A" then (697., 1633.)
      elsif c == "4" then (770., 1209.)
      elsif c == "5" then (770., 1336.)
      elsif c == "6" then (770., 1477.)
      elsif c == "B" then (770., 1633.)
      elsif c == "7" then (852., 1209.)
      elsif c == "8" then (852., 1336.)
      elsif c == "9" then (852., 1477.)
      elsif c == "C" then (852., 1633.)
      elsif c == "*" then (941., 1209.)
      elsif c == "0" then (941., 1336.)
      elsif c == "#" then (941., 1477.)
      elsif c == "D" then (941., 1633.)
      else
        (0., 0.)
      end

    s = add([sine(row, duration=duration), sine(col, duration=duration)])
    l := blank(duration=delay)::l()
    l := s::l()
  end

  l = list.rev(l())
  sequence(l)
end

# Mixing table controllable via source methods and optional
# server/telnet commands.
# @flag extra
# @category Source / Audio processing
# @param ~id Force the value of the source ID.
# @param ~register_server_commands Register corresponding server commands
def mix(~id=null(), ~register_server_commands=true, sources) =
  id = string.id.default(default="mixer", id)
  inputs =
    list.map(
      fun (s) ->
        begin
          volume = ref(1.)
          is_selected = ref(false)
          is_single = ref(false)
          {volume=volume, selected=is_selected, single=is_single, source=s}
        end,
      sources
    )

  insert_metadata_fn = ref(fun (_) -> ())
  sources =
    list.map(
      fun (input) ->
        begin
          s = amplify(input.volume, input.source)
          s =
            source.on_track(
              s, fun (_) -> if input.single() then input.selected := false end
            )

          s =
            source.on_metadata(
              s,
              fun (m) ->
                begin
                  fn = insert_metadata_fn()
                  fn(m)
                end
            )

          switch([(input.selected, s)])
        end,
      inputs
    )

  s = add(sources)
  let {metadata = _, ...tracks} = source.tracks(s)
  s = source(tracks)
  s = insert_metadata(s)
  insert_metadata_fn := s.insert_metadata
  let {track_marks = _, ...tracks} = source.tracks(s)
  s = source(id=id, tracks)
  if
    register_server_commands
  then
    def status(input) =
      "ready=#{source.is_ready(
        input.source
      )} selected=#{input.selected()} single=#{input.single()} volume=#{int_of_float(
        input.volume() * 100.
      )}% remaining=#{source.remaining(input.source)}"
    end

    server.register(
      namespace=source.id(s),
      description="Skip current track on all enabled sources.",
      "skip",
      fun (_) ->
        begin
          list.iter(
            fun (input) ->
              if input.selected() then source.skip(input.source) end,
            inputs
          )

          "OK"
        end
    )

    server.register(
      namespace=source.id(s),
      description="Set volume for a given source.",
      usage="volume <source nb> <vol%>",
      "volume",
      fun (v) ->
        begin
          try
            let [i, v] = r/\s/.split(v)
            input = list.nth(inputs, int_of_string(i))
            input.volume := float_of_string(v)
            status(input)
          catch _ do
            "Usage: volume <source nb> <vol%>"
          end
        end
    )

    server.register(
      namespace=source.id(s),
      description="Enable/disable a source.",
      usage="select <source nb> <true|false>",
      "select",
      fun (arg) ->
        begin
          try
            let [i, b] = r/\s/.split(arg)
            input = list.nth(inputs, int_of_string(i))
            input.selected := (b == "true")
            status(input)
          catch _ do
            "Usage: select <source nb> <true|false>"
          end
        end
    )

    server.register(
      namespace=source.id(s),
      description="Enable/disable automatic stop at the end of track.",
      usage="single <source nb> <true|false>",
      "single",
      fun (arg) ->
        begin
          try
            let [i, b] = r/\s/.split(arg)
            input = list.nth(inputs, int_of_string(i))
            input.single := (b == "true")
            status(input)
          catch _ do
            "Usage: single <source nb> <true|false>"
          end
        end
    )

    server.register(
      namespace=source.id(s),
      description="Display current status.",
      "status",
      fun (i) ->
        begin
          try
            status(list.nth(inputs, int_of_string(i)))
          catch _ do
            "Usage: status <source nb>"
          end
        end
    )

    server.register(
      namespace=source.id(s),
      description="Print the list of input sources.",
      "inputs",
      fun (_) ->
        string.concat(
          separator=" ",
          list.map(fun (input) -> source.id(input.source), inputs)
        )
    )
  end

  s.{inputs=inputs}
end