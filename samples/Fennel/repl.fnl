;; This module is the read/eval/print loop; for coding Fennel interactively.

;; The most complex thing it does is locals-saving, which allows locals to be
;; preserved in between "chunks"; by default Lua throws away all locals after
;; evaluating each piece of input.

(local utils (require :fennel.utils))
(local parser (require :fennel.parser))
(local compiler (require :fennel.compiler))
(local specials (require :fennel.specials))

(fn default-read-chunk [parser-state]
  (io.write (if (< 0 parser-state.stack-size) ".." ">> "))
  (io.flush)
  (let [input (io.read)]
    (and input (.. input "\n"))))

(fn default-on-values [xs]
  (io.write (table.concat xs "\t"))
  (io.write "\n"))

;; fnlfmt: skip
(fn default-on-error [errtype err lua-source]
  (io.write
   (match errtype
     "Lua Compile" (.. "Bad code generated - likely a bug with the compiler:\n"
                       "--- Generated Lua Start ---\n"
                       lua-source
                       "--- Generated Lua End ---\n")
     "Runtime" (.. (compiler.traceback (tostring err) 4) "\n")
     _ (: "%s error: %s\n" :format errtype (tostring err)))))

(local save-source (table.concat ["local ___i___ = 1"
                                  "while true do"
                                  " local name, value = debug.getlocal(1, ___i___)"
                                  " if(name and name ~= \"___i___\") then"
                                  " ___replLocals___[name] = value"
                                  " ___i___ = ___i___ + 1"
                                  " else break end end"]
                                 "\n"))

(fn splice-save-locals [env lua-source]
  (set env.___replLocals___ (or env.___replLocals___ {}))
  (let [spliced-source []
        bind "local %s = ___replLocals___['%s']"]
    (each [line (lua-source:gmatch "([^\n]+)\n?")]
      (table.insert spliced-source line))
    (each [name (pairs env.___replLocals___)]
      (table.insert spliced-source 1 (bind:format name name)))
    (when (and (< 1 (length spliced-source))
               (: (. spliced-source (length spliced-source)) :match
                  "^ *return .*$"))
      (table.insert spliced-source (length spliced-source) save-source))
    (table.concat spliced-source "\n")))

(fn completer [env scope text]
  (let [matches []
        input-fragment (text:gsub ".*[%s)(]+" "")]
    (var stop-looking? false)

    (fn add-partials [input tbl prefix] ; add partial key matches in tbl
      (each [k (utils.allpairs tbl)]
        (let [k (if (or (= tbl env) (= tbl env.___replLocals___))
                    (. scope.unmanglings k)
                    k)]
          (when (and (< (length matches) 2000)
                     ; stop explosion on too many items
                     (= (type k) :string) (= input (k:sub 0 (length input))))
            (table.insert matches (.. prefix k))))))

    (fn add-matches [input tbl prefix] ; add matches, descending into tbl fields
      (let [prefix (if prefix (.. prefix ".") "")]
        (if (not (input:find "%.")) ; no more dots, so add matches
            (add-partials input tbl prefix)
            (let [(head tail) (input:match "^([^.]+)%.(.*)")
                  raw-head (if (or (= tbl env) (= tbl env.___replLocals___))
                               (. scope.manglings head)
                               head)]
              (when (= (type (. tbl raw-head)) :table)
                (set stop-looking? true)
                (add-matches tail (. tbl raw-head) (.. prefix head)))))))

    (each [_ source (ipairs [scope.specials scope.macros
                             (or env.___replLocals___ []) env env._G])]
      (add-matches input-fragment source)
      ;; bootstrap compiler doesn't yet know how to :until
      (when stop-looking? (lua :break)))
    matches))

(local commands {})

(fn command? [input]
  (input:match "^%s*,"))

(fn command-docs []
  (table.concat (icollect [name f (pairs commands)]
                  (: "  ,%s - %s" :format name
                     (or (compiler.metadata:get f :fnl/docstring) :undocumented)))
                "\n"))

;; fnlfmt: skip
(fn commands.help [_ _ on-values]
  "Show this message."
  (on-values [(.. "Welcome to Fennel.
This is the REPL where you can enter code to be evaluated.
You can also run these repl commands:
" (command-docs) "
  ,exit - Leave the repl.
Use (doc something) to see descriptions for individual macros and special forms.
For more information about the language, see https://fennel-lang.org/reference")]))

;; Can't rely on metadata being enabled at load time for Fennel's own internals.
(compiler.metadata:set commands.help :fnl/docstring "Show this message.")

(fn reload [module-name env on-values on-error]
  ;; Sandbox the reload inside the limited environment, if present.
  (match (pcall (specials.load-code "return require(...)" env) module-name)
    (true old) (let [_ (tset package.loaded module-name nil)
                     (ok new) (pcall require module-name)
                     ;; keep the old module if reload failed
                     new (if (not ok)
                             (do
                               (on-values [new])
                               old)
                             new)]
                 ;; if the module isn't a table then we can't make changes
                 ;; which affect already-loaded code, but if it is then we
                 ;; should splice new values into the existing table and
                 ;; remove values that are gone.
                 (when (and (= (type old) :table) (= (type new) :table))
                   (each [k v (pairs new)]
                     (tset old k v))
                   (each [k (pairs old)]
                     (when (= nil (. new k))
                       (tset old k nil)))
                   (tset package.loaded module-name old))
                 (on-values [:ok]))
    (false msg) (on-error :Runtime (pick-values 1 (msg:gsub "\n.*" "")))))

(fn commands.reload [env read on-values on-error]
  (match (pcall read)
    (true true module-sym) (reload (tostring module-sym) env on-values on-error)
    (false ?parse-ok ?msg) (on-error :Parse (or ?msg ?parse-ok))))

(compiler.metadata:set commands.reload :fnl/docstring
                       "Reload the specified module.")

(fn commands.reset [env _ on-values]
  (set env.___replLocals___ {})
  (on-values [:ok]))

(compiler.metadata:set commands.reset :fnl/docstring
                       "Erase all repl-local scope.")

(fn commands.complete [env read on-values on-error scope]
  (match (pcall read)
    (true true input) (on-values (completer env scope (tostring input)))
    (_ _ ?msg) (on-error :Parse (or ?msg "Couldn't parse completion input."))))

(compiler.metadata:set commands.complete :fnl/docstring
                       "Print all possible completions for a given input.")

(fn load-plugin-commands []
  (when (and utils.root utils.root.options utils.root.options.plugins)
    (each [_ plugin (ipairs utils.root.options.plugins)]
      (each [name f (pairs plugin)]
        ;; first function to provide a command should win
        (match (name:match "^repl%-command%-(.*)")
          cmd-name (tset commands cmd-name (or (. commands cmd-name) f)))))))

(fn run-command [input read loop env on-values on-error scope]
  (load-plugin-commands)
  (let [command-name (input:match ",([^%s/]+)")]
    (match (. commands command-name)
      command (command env read on-values on-error scope)
      _ (when (not= :exit command-name)
          (on-values ["Unknown command" command-name])))
    (when (not= :exit command-name)
      (loop))))

(fn repl [options]
  (let [old-root-options utils.root.options
        env (if options.env
                (specials.wrap-env options.env)
                (setmetatable {} {:__index (or (rawget _G :_ENV) _G)}))
        save-locals? (and (not= options.saveLocals false) env.debug
                          env.debug.getlocal)
        opts {}
        _ (each [k v (pairs options)]
            (tset opts k v))
        read-chunk (or opts.readChunk default-read-chunk)
        on-values (or opts.onValues default-on-values)
        on-error (or opts.onError default-on-error)
        pp (or opts.pp tostring) ;; make parser
        (byte-stream clear-stream) (parser.granulate read-chunk)
        chars []
        (read reset) (parser.parser (fn [parser-state]
                                      (let [c (byte-stream parser-state)]
                                        (table.insert chars c)
                                        c)))
        scope (compiler.make-scope)]
    ;; use metadata unless we've specifically disabled it
    (set opts.useMetadata (not= options.useMetadata false))
    (when (= opts.allowedGlobals nil)
      (set opts.allowedGlobals (specials.current-global-names opts.env)))
    (when opts.registerCompleter
      (opts.registerCompleter (partial completer env scope)))

    (fn print-values [...]
      (let [vals [...]
            out []]
        (set (env._ env.__) (values (. vals 1) vals))
        ;; utils.map won't work here because of sparse tables
        (for [i 1 (select "#" ...)]
          (table.insert out (pp (. vals i))))
        (on-values out)))

    (fn loop []
      (each [k (pairs chars)]
        (tset chars k nil))
      (let [(ok parse-ok? x) (pcall read)
            src-string (string.char ((or table.unpack _G.unpack) chars))]
        (set utils.root.options opts)
        (if (not ok)
            (do
              (on-error :Parse parse-ok?)
              (clear-stream)
              (reset)
              (loop))
            (command? src-string)
            (run-command src-string read loop env on-values on-error scope)
            (when parse-ok? ; if this is false, we got eof
              (match (pcall compiler.compile x
                            {:correlate opts.correlate
                             :source src-string
                             : scope
                             :useMetadata opts.useMetadata
                             :moduleName opts.moduleName
                             :assert-compile opts.assert-compile
                             :parse-error opts.parse-error
                             :useBitLib opts.useBitLib})
                (false msg) (do
                              (clear-stream)
                              (on-error :Compile msg))
                (true src) (let [src (if save-locals?
                                         (splice-save-locals env src)
                                         src)]
                             (match (pcall specials.load-code src env)
                               (false msg) (do
                                             (clear-stream)
                                             (on-error "Lua Compile" msg src))
                               (_ chunk) (xpcall #(print-values (chunk))
                                                 (partial on-error :Runtime)))))
              (set utils.root.options old-root-options)
              (loop)))))

    (loop)))
