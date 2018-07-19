fs = require 'fs'
path = require 'path'

root = path.resolve '/'
current_dir = __dirname
filename = 'input.txt'
dirname = 'docs'

local_file = path.join current_dir, filename
local_dir = path.join current_dir, dirname

root_file = path.join root, filename
root_dir = path.join root, dirname

for p in [ local_file, local_dir, root_file, root_dir ] then do ( p ) ->
    fs.exists p, ( p_exists ) ->
        unless p_exists
            console.log "#{ p } does not exist."
        else then fs.stat p, ( error, stat ) ->
            console.log "#{ p } exists and is a #{ if stat.isFile() then 'file' else then 'directory' }."
