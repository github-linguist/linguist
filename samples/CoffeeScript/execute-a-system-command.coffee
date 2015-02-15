{ spawn } = require 'child_process'

ls = spawn 'ls'

ls.stdout.on 'data', ( data ) -> console.log "Output: #{ data }"

ls.stderr.on 'data', ( data ) -> console.error "Error: #{ data }"

ls.on 'close', -> console.log "'ls' has finished executing."
