# runs under node.js
dns = require 'dns'

dns.resolve4 'www.kame.net', (err, addresses) ->
  console.log 'IP4'
  console.log addresses

dns.resolve6 'www.kame.net', (err, addresses) ->
  console.log 'IP6'
  console.log addresses
