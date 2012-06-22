dnsserver = require "dnsserver"

exports.Server = class Server extends dnsserver.Server
  NS_T_A            = 1
  NS_T_NS           = 2
  NS_T_CNAME        = 5
  NS_T_SOA          = 6
  NS_C_IN           = 1
  NS_RCODE_NXDOMAIN = 3

  constructor: (domain, @rootAddress) ->
    super
    @domain = domain.toLowerCase()
    @soa = createSOA @domain
    @on "request", @handleRequest

  handleRequest: (req, res) =>
    question  = req.question
    subdomain = @extractSubdomain question.name

    if subdomain? and isARequest question
      res.addRR question.name, NS_T_A, NS_C_IN, 600, subdomain.getAddress()
    else if subdomain?.isEmpty() and isNSRequest question
      res.addRR question.name, NS_T_SOA, NS_C_IN, 600, @soa, true
    else
      res.header.rcode = NS_RCODE_NXDOMAIN

    res.send()

  extractSubdomain: (name) ->
    Subdomain.extract name, @domain, @rootAddress

  isARequest = (question) ->
    question.type is NS_T_A and question.class is NS_C_IN

  isNSRequest = (question) ->
    question.type is NS_T_NS and question.class is NS_C_IN

  createSOA = (domain) ->
    mname   = "ns-1.#{domain}"
    rname   = "hostmaster.#{domain}"
    serial  = parseInt new Date().getTime() / 1000
    refresh = 28800
    retry   = 7200
    expire  = 604800
    minimum = 3600
    dnsserver.createSOA mname, rname, serial, refresh, retry, expire, minimum

exports.createServer = (domain, address = "127.0.0.1") ->
  new Server domain, address

exports.Subdomain = class Subdomain
  @extract: (name, domain, address) ->
    return unless name
    name = name.toLowerCase()
    offset = name.length - domain.length

    if domain is name.slice offset
      subdomain = if 0 >= offset then null else name.slice 0, offset - 1
      new constructor subdomain, address if constructor = @for subdomain

  @for: (subdomain = "") ->
    if IPAddressSubdomain.pattern.test subdomain
      IPAddressSubdomain
    else if EncodedSubdomain.pattern.test subdomain
      EncodedSubdomain
    else
      Subdomain

  constructor: (@subdomain, @address) ->
    @labels = subdomain?.split(".") ? []
    @length = @labels.length

  isEmpty: ->
    @length is 0

  getAddress: ->
    @address

class IPAddressSubdomain extends Subdomain
  @pattern = /// (^|\.)
    ((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}
    (25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
  $ ///

  getAddress: ->
    @labels.slice(-4).join "."

class EncodedSubdomain extends Subdomain
  @pattern = /(^|\.)[a-z0-9]{1,7}$/

  getAddress: ->
    decode @labels[@length - 1]

exports.encode = encode = (ip) ->
  value = 0
  for byte, index in ip.split "."
    value += parseInt(byte, 10) << (index * 8)
  (value >>> 0).toString 36

PATTERN = /^[a-z0-9]{1,7}$/

exports.decode = decode = (string) ->
  return unless PATTERN.test string
  value = parseInt string, 36
  ip = []
  for i in [1..4]
    ip.push value & 0xFF
    value >>= 8
  ip.join "."
