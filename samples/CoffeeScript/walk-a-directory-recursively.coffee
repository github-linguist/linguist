fs = require 'fs'

walk = (dir, f_match, f_visit) ->
  _walk = (dir) ->
    fns = fs.readdirSync dir
    for fn in fns
      fn = dir + '/' + fn
      if f_match fn
        f_visit fn
      if fs.statSync(fn).isDirectory()
        _walk fn
  _walk(dir)

dir = '..'
matcher = (fn) -> fn.match /\.coffee/
action = console.log
walk dir, matcher, action
