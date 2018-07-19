#!/usr/bin/env coffee

main = () ->
  program = __filename
  console.log "Program: " + program

if not module.parent then main()
