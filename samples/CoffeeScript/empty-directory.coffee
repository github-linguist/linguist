fs = require 'fs'

is_empty_dir = (dir) ->
  throw Error "#{dir} is not a dir" unless fs.statSync(dir).isDirectory()
  # readdirSync does not return . or ..
  fns = fs.readdirSync dir
  fns.length == 0
