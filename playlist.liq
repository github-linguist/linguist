# Play a list of files.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~check_next Function used to filter next tracks. A candidate track is \
#   only validated if the function returns true on it. The function is called \
#   before resolution, hence metadata will only be available for requests \
#   corresponding to local files. This is typically used to avoid repetitions, \
#   but be careful: if the function rejects all attempts, the playlist will \
#   enter into a consuming loop and stop playing anything.
# @param ~prefetch How many requests should be queued in advance.
# @param ~loop Loop on the playlist.
# @param ~mode Play the files in the playlist either in the order ("normal" mode), \
#  or shuffle the playlist each time it is loaded, and play it in this order for a \
#  whole round ("randomize" mode), or pick a random file in the playlist each time \
#  ("random" mode).
# @param ~native Use native implementation, when available.
# @param ~on_loop Function executed when the playlist is about to loop.
# @param ~on_done Function executed when the playlist is finished.
# @param ~max_fail When this number of requests fail to resolve, the whole playlists is considered as failed and `on_fail` is called.
# @param ~on_fail Function executed when too many requests failed and returning the contents of a fixed playlist.
# @param ~timeout Timeout (in sec.) for a single download.
# @param playlist Playlist.
# @method reload Reload the playlist with given list of songs.
# @method remaining_files Songs remaining to be played.
def playlist.list(
  ~id=null(),
  ~check_next=null(),
  ~prefetch=1,
  ~loop=true,
  ~mode="normal",
  ~native=false,
  ~on_loop={()},
  ~on_done={()},
  ~max_fail=10,
  ~on_fail=null(),
  ~timeout=20.,
  playlist
) =
  ignore(native)
  id = string.id.default(default="playlist.list", id)
  mode =
    if
      not list.mem(mode, ["normal", "random", "randomize"])
    then
      log.severe(label=id, "Invalid mode: #{mode}")
      "randomize"
    else
      mode
    end

  check_next = check_next ?? fun (_) -> true
  should_stop = ref(false)
  on_shutdown({should_stop.set(true)})
  on_fail =
    null.map(
      fun (on_fail) -> {if not should_stop() then on_fail() else [] end},
      on_fail
    )


  # Original playlist when loaded
  playlist_orig = ref(playlist)

  # Randomize the playlist if necessary
  def randomize(p) =
    if mode == "randomize" then list.shuffle(p) else p end
  end

  # Current remaining playlist
  playlist = ref(randomize(playlist))

  # A reference to know if the source has been stopped
  has_stopped = ref(false)

  # Delay the creation of next after the source because we need it to resolve
  # requests at the right content type.
  next_fun = ref(fun () -> null())

  def next() =
    f = next_fun()
    f()
  end

  # Instantiate the source
  default =
    request.dynamic(
      id=id,
      prefetch=prefetch,
      timeout=timeout,
      retry_delay=1.,
      available={not has_stopped()},
      next
    )

  s =
%ifdef native
    if native then stdlib_native.request.dynamic(id=id, next) else default end
%else
    default
%endif

  source.set_name(s, "playlist.list.reloadable")

  # The reload function
  def reload(~empty_queue=true, p) =
    log.debug(label=id, "Reloading playlist.")
    playlist_orig := p
    playlist := randomize(playlist_orig())
    has_stopped := false
    if
      empty_queue
    then
      q = s.queue()
      s.set_queue([])
      list.iter(request.destroy, q)
      ignore(s.fetch())
    end
  end

  # When we have more than max_fail failures in a row, we wait for 1 second
  # before trying again in order to avoid infinite loops.
  failed_count = ref(0)
  failed_time = ref(0.)

  # The (real) next function
  def rec next() =
    if
      loop and list.is_empty(playlist())
    then
      on_loop()

      # The above function might have reloaded the playlist
      if
        list.is_empty(playlist())
      then
        playlist := randomize(playlist_orig())
      end
    end

    file =
      if
        list.length(playlist()) > 0
      then
        if
          mode == "random"
        then
          n = random.int(min=0, max=list.length(playlist()))
          list.nth(default="", playlist(), n)
        else
          ret = list.hd(default="", playlist())
          playlist := list.tl(playlist())
          ret
        end
      else
        # Playlist finished
        if
          not has_stopped()
        then
          has_stopped := true
          log.info(label=id, "Playlist stopped.")
          on_done()
        end

        ""
      end

    if
      file == "" or (failed_count() >= max_fail and time() < failed_time() + 1.)
    then
      # Playlist failed too many times recently, don't try next for now.
      null()
    else
      log.debug(label=id, "Next song will be \"#{file}\".")
      r = request.create(file)
      if
        not request.resolve(content_type=s, r)
      then
        log.info(label=id, "Could not resolve request: #{request.uri(r)}.")
        request.destroy(r)
        ref.incr(failed_count)

        # Playlist failed, call handler.
        if
          failed_count() < max_fail
        then
          log.info(label=id, "Playlist failed.")
          if
            null.defined(on_fail)
          then
            f = null.get(on_fail)
            reload(f())
          end
        end

        failed_time := time()
        (next() : request?)
      else
        failed_count := 0
        if
          check_next(r)
        then
          r
        else
          log.info(
            label=id, "Request #{request.uri(r)} rejected by check_next."
          )

          request.destroy(r)
          next()
        end
      end
    end
  end

  next_fun := next

  # List of songs remaining to be played
  def remaining_files() =
    playlist()
  end

  # Return
  s.{reload=reload, remaining_files=remaining_files}
end