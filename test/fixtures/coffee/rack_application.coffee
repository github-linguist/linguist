# The `RackApplication` class is responsible for managing a
# [Nack](http://josh.github.com/nack/) pool for a given Rack
# application. Incoming HTTP requests are dispatched to
# `RackApplication` instances by an `HttpServer`, where they are
# subsequently handled by a pool of Nack worker processes. By default,
# Pow tells Nack to use a maximum of two worker processes per
# application, but this can be overridden with the configuration's
# `workers` option.
#
# Before creating the Nack pool, Pow executes the `.powrc` and
# `.powenv` scripts if they're present in the application root,
# captures their environment variables, and passes them along to the
# Nack worker processes. This lets you modify your `RUBYOPT` to use
# different Ruby options, for example.
#
# If [rvm](http://rvm.beginrescueend.com/) is installed and an
# `.rvmrc` file is present in the application's root, Pow will load
# both before creating the Nack pool. This makes it easy to run an
# app with a specific version of Ruby.
#
# Nack workers remain running until they're killed, restarted (by
# touching the `tmp/restart.txt` file in the application root), or
# until the application has not served requests for the length of time
# specified in the configuration's `timeout` option (15 minutes by
# default).

async = require "async"
fs    = require "fs"
nack  = require "nack"

{bufferLines, pause, sourceScriptEnv} = require "./util"
{join, exists, basename, resolve} = require "path"

module.exports = class RackApplication
  # Create a `RackApplication` for the given configuration and
  # root path. The application begins life in the uninitialized
  # state.
  constructor: (@configuration, @root, @firstHost) ->
    @logger = @configuration.getLogger join "apps", basename @root
    @readyCallbacks = []
    @quitCallbacks  = []
    @statCallbacks  = []

  # Queue `callback` to be invoked when the application becomes ready,
  # then start the initialization process. If the application's state
  # is ready, the callback is invoked immediately.
  ready: (callback) ->
    if @state is "ready"
      callback()
    else
      @readyCallbacks.push callback
      @initialize()

  # Tell the application to quit and queue `callback` to be invoked
  # when all workers have exited. If the application has already quit,
  # the callback is invoked immediately.
  quit: (callback) ->
    if @state
      @quitCallbacks.push callback if callback
      @terminate()
    else
      callback?()

  # Stat `tmp/restart.txt` in the application root and invoke the
  # given callback with a single argument indicating whether or not
  # the file has been touched since the last call to
  # `queryRestartFile`.
  queryRestartFile: (callback) ->
    fs.stat join(@root, "tmp/restart.txt"), (err, stats) =>
      if err
        @mtime = null
        callback false
      else
        lastMtime = @mtime
        @mtime = stats.mtime.getTime()
        callback lastMtime isnt @mtime

  # Check to see if `tmp/always_restart.txt` is present in the
  # application root, and set the pool's `runOnce` option
  # accordingly. Invoke `callback` when the existence check has
  # finished. (Multiple calls to this method are aggregated.)
  setPoolRunOnceFlag: (callback) ->
    unless @statCallbacks.length
      exists join(@root, "tmp/always_restart.txt"), (alwaysRestart) =>
        @pool.runOnce = alwaysRestart
        statCallback() for statCallback in @statCallbacks
        @statCallbacks = []

    @statCallbacks.push callback

  # Collect environment variables from `.powrc` and `.powenv`, in that
  # order, if present. The idea is that `.powrc` files can be checked
  # into a source code repository for global configuration, leaving
  # `.powenv` free for any necessary local overrides.
  loadScriptEnvironment: (env, callback) ->
    async.reduce [".powrc", ".envrc", ".powenv"], env, (env, filename, callback) =>
      exists script = join(@root, filename), (scriptExists) ->
        if scriptExists
          sourceScriptEnv script, env, callback
        else
          callback null, env
    , callback

  # If `.rvmrc` and `$HOME/.rvm/scripts/rvm` are present, load rvm,
  # source `.rvmrc`, and invoke `callback` with the resulting
  # environment variables. If `.rvmrc` is present but rvm is not
  # installed, invoke `callback` without sourcing `.rvmrc`.
  # Before loading rvm, Pow invokes a helper script that shows a
  # deprecation notice if it has not yet been displayed.
  loadRvmEnvironment: (env, callback) ->
    exists script = join(@root, ".rvmrc"), (rvmrcExists) =>
      if rvmrcExists
        exists rvm = @configuration.rvmPath, (rvmExists) =>
          if rvmExists
            libexecPath = resolve "#{__dirname}/../libexec"
            before = """
              '#{libexecPath}/pow_rvm_deprecation_notice' '#{[@firstHost]}'
              source '#{rvm}' > /dev/null
            """.trim()
            sourceScriptEnv script, env, {before}, callback
          else
            callback null, env
      else
        callback null, env

  # Stat `tmp/restart.txt` to cache its mtime, then load the
  # application's full environment from `.powrc`, `.powenv`, and
  # `.rvmrc`.
  loadEnvironment: (callback) ->
    @queryRestartFile =>
      @loadScriptEnvironment @configuration.env, (err, env) =>
        if err then callback err
        else @loadRvmEnvironment env, (err, env) =>
          if err then callback err
          else callback null, env

  # Begin the initialization process if the application is in the
  # uninitialized state. (If the application is terminating, queue a
  # call to `initialize` after all workers have exited.)
  initialize: ->
    if @state
      if @state is "terminating"
        @quit => @initialize()
      return

    @state = "initializing"

    # Load the application's environment. If an error is raised or
    # either of the environment scripts exits with a non-zero status,
    # reset the application's state and log the error.
    @loadEnvironment (err, env) =>
      if err
        @state = null
        @logger.error err.message
        @logger.error "stdout: #{err.stdout}"
        @logger.error "stderr: #{err.stderr}"

      # Set the application's state to ready. Then create the Nack
      # pool instance using the `workers` and `timeout` options from
      # the application's environment or the global configuration.
      else
        @state = "ready"

        @pool = nack.createPool join(@root, "config.ru"),
          env:  env
          size: env?.POW_WORKERS ? @configuration.workers
          idle: (env?.POW_TIMEOUT ? @configuration.timeout) * 1000

        # Log the workers' stderr and stdout, and log each worker's
        # PID as it spawns and exits.
        bufferLines @pool.stdout, (line) => @logger.info line
        bufferLines @pool.stderr, (line) => @logger.warning line

        @pool.on "worker:spawn", (process) =>
          @logger.debug "nack worker #{process.child.pid} spawned"

        @pool.on "worker:exit", (process) =>
          @logger.debug "nack worker exited"

      # Invoke and remove all queued callbacks, passing along the
      # error, if any.
      readyCallback err for readyCallback in @readyCallbacks
      @readyCallbacks = []

  # Begin the termination process. (If the application is initializing,
  # wait until it is ready before shutting down.)
  terminate: ->
    if @state is "initializing"
      @ready => @terminate()

    else if @state is "ready"
      @state = "terminating"

      # Instruct all workers to exit. After the processes have
      # terminated, reset the application's state, then invoke and
      # remove all queued callbacks.
      @pool.quit =>
        @state = null
        @mtime = null
        @pool = null

        quitCallback() for quitCallback in @quitCallbacks
        @quitCallbacks = []

  # Handle an incoming HTTP request. Wait until the application is in
  # the ready state, restart the workers if necessary, then pass the
  # request along to the Nack pool. If the Nack worker raises an
  # exception handling the request, reset the application.
  handle: (req, res, next, callback) ->
    resume = pause req
    @ready (err) =>
      return next err if err
      @setPoolRunOnceFlag =>
        @restartIfNecessary =>
          req.proxyMetaVariables =
            SERVER_PORT: @configuration.dstPort.toString()
          try
            @pool.proxy req, res, (err) =>
              @quit() if err
              next err
          finally
            resume()
            callback?()

  # Terminate the application, re-initialize it, and invoke the given
  # callback when the application's state becomes ready.
  restart: (callback) ->
    @quit =>
      @ready callback

  # Restart the application if `tmp/restart.txt` has been touched
  # since the last call to this function.
  restartIfNecessary: (callback) ->
    @queryRestartFile (mtimeChanged) =>
      if mtimeChanged
        @restart callback
      else
        callback()

  # Append RVM autoload boilerplate to the application's `.powrc`
  # file. This is called by the RVM deprecation notice mini-app.
  writeRvmBoilerplate: ->
    powrc = join @root, ".powrc"
    boilerplate = @constructor.rvmBoilerplate

    fs.readFile powrc, "utf8", (err, contents) ->
      contents ?= ""
      if contents.indexOf(boilerplate) is -1
        fs.writeFile powrc, "#{boilerplate}\n#{contents}"

  @rvmBoilerplate: """
    if [ -f "$rvm_path/scripts/rvm" ] && [ -f ".rvmrc" ]; then
      source "$rvm_path/scripts/rvm"
      source ".rvmrc"
    fi
  """
