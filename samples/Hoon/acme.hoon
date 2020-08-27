/-  asn1, hall
/+  base64, der, primitive-rsa, *pkcs, *jose, default-agent, verb
=,  eyre
=*  rsa  primitive-rsa
::
|%
::  +en-base64url: url-safe base64 encoding, without padding
::
++  en-base64url
  ~(en base64 | &)
::  +de-base64url: url-safe base64 decoding, without padding
::
++  de-base64url
  ~(de base64 | &)
::  +join-turf
::
++  join-turf
  |=  hot=(list turf)
  ^-  cord
  %+  rap  3
  %-  (bake join ,[cord wain])
  [', ' (turn hot en-turf:html)]
::  |octn: encode/decode unsigned atoms as big-endian octet stream
::
++  octn
  |%
  ++  en  |=(a=@u `octs`[(met 3 a) (swp 3 a)])
  ++  de  |=(a=octs `@u`(rev 3 p.a q.a))
  --
::  |body: acme api response body types
::
++  body
  |%
  +$  acct  [wen=@t sas=@t]
  ::
  +$  order
    $:  exp=@t
        sas=@t
        aut=(list purl)
        fin=(unit purl)
        cer=(unit purl)
    ==
  ::
  +$  auth
    $:  dom=turf
        sas=@t
        exp=@t
        cal=challenge
    ==
  ::
  +$  challenge  [typ=@t sas=@t url=purl tok=@t err=(unit error)]
  ::
  +$  error  [type=@t detail=@t]
  --
::
::  |grab: acme api response json reparsers
::
++  grab
  =,  dejs:format
  |%
  ::  +json-purl: parse url
  ::
  ++  json-purl  (su auri:de-purl:html)
  ::  +json-date: parse iso-8601
  ::
  ::    XX actually parse
  ::
  ++  json-date  so
  ::  +directory: parse ACME service directory
  ::
  ++  directory
    %-  ot
    :~  'newAccount'^json-purl
        'newNonce'^json-purl
        'newOrder'^json-purl
        'revokeCert'^json-purl
        'keyChange'^json-purl
    ==
  ::  +acct: parse ACME service account
  ::
  ++  acct
    ^-  $-(json acct:body)
    ::  ignoring key, contact, initialIp
    ::
    (ot 'createdAt'^json-date 'status'^so ~)
  ::  +order: parse certificate order
  ::
  ++  order
    ^-  $-(json order:body)
    %-  ou
    :~  'expires'^(un json-date)
        'status'^(un so)
        'authorizations'^(uf ~ (ar json-purl))
        'finalize'^(uf ~ (mu json-purl))
        'certificate'^(uf ~ (mu json-purl))
    ==
  ::  +auth: parse authorization
  ::
  ++  auth
    =>  |%
        ::  +iden: extract +turf from service identifier
        ::
        ++  iden
          |=  [typ=@t hot=host]
          ^-  turf
          ?>(&(?=(%dns typ) ?=([%& *] hot)) p.hot)
        ::  +http-trial: extract %http-01 challenge
        ::
        ++  trial
          |=  a=(list challenge:body)
          ^-  challenge:body
          =/  b  (skim a |=([typ=@t *] ?=(%http-01 typ)))
          ?>(?=(^ b) i.b)
        --
    ^-  $-(json auth:body)
    %-  ot
    :~  'identifier'^(cu iden (ot type+so value+(su thos:de-purl:html) ~))
        'status'^so
        'expires'^json-date
        'challenges'^(cu trial (ar challenge))
    ==
  ::  +challenge: parse domain validation challenge
  ::
  ++  challenge
    ^-  $-(json challenge:body)
    %-  ou
    :~  'type'^(un so)
        'status'^(un so)
        'url'^(un json-purl)
        'token'^(un so)
        'error'^(uf ~ (mu error))
    ==
  ::  +error: parse ACME service error response
  ::
  ++  error
    ^-  $-(json error:body)
    (ot type+so detail+so ~)
  --
--
::
::::  acme state
::
|%
::  +card: output effect payload
::
+$  card  card:agent:gall
::  +nonce-next: next effect to emit upon receiving nonce
::
+$  nonce-next
  $?  %register
      %new-order
      %finalize-order
      %finalize-trial
  ==
::  +acct: an ACME service account
::
+$  acct
  $:  ::  key: account keypair
      ::
      key=key:rsa
      ::  reg: account registration
      ::
      ::    XX wen=@da once parser is fixed
      ::
      reg=(unit [wen=@t kid=@t])
  ==
::  +config: finalized configuration
::
+$  config
  $:  ::  dom: domains
      ::
      dom=(set turf)
      ::  key: certificate keypair
      ::
      key=key:rsa
      ::  cer: signed certificate
      ::
      cer=wain
      ::  exp: expiration date
      ::
      exp=@da
      ::  dor: source ACME service order URL
      ::
      dor=purl
  ==
::  +trial: domain validation challenge
::
+$  trial
  $%  ::  %http only for now
      ::
      $:  %http
          ::  ego: ACME service challenge url
          ::
          ego=purl
          ::  tok: challenge token
          ::
          tok=@t
          ::  sas: challenge status
          ::
          sas=?(%recv %pend %auth)
  ==  ==
::  +auth: domain authorization
::
+$  auth
  $:  ::  ego: ACME service authorization url
      ::
      ego=purl
      ::  dom: domain under authorization
      ::
      dom=turf
      ::  cal: domain validation challenge
      ::
      cal=trial
  ==
::  +order-auth: domain authorization state for order processing
::
+$  order-auth
  $:  ::  pending: remote authorization urls
      ::
      pending=(list purl)
      ::  active: authorization in progress
      ::
      active=(unit [idx=@ auth])
      ::  done: finalized authorizations (XX or failed?)
      ::
      done=(list auth)
  ==
::  +order: ACME certificate order
::
+$  order
  $:  ::  dom: domains
      ::
      dom=(set turf)
      ::  try: attempt number
      ::
      try=@ud
      ::  sas: order state
      ::
      sas=$@(%wake [%rest wen=@da])
      ::  exp: expiration date
      ::
      ::    XX @da once ISO-8601 parser
      ::
      exp=@t
      ::  ego: ACME service order url
      ::
      ego=purl
      ::  fin: ACME service order finalization url
      ::
      fin=purl
      ::  key: certificate keypair
      ::
      key=key:rsa
      ::  csr: DER-encoded PKCS10 certificate signing request
      ::
      csr=@ux
      ::  aut: authorizations required by this order
      ::
      aut=order-auth
  ==
::  +history: archive of past ACME service interactions
::
+$  history
  $:  ::  act: list of revoked account keypairs
      ::
      act=(list acct)
      ::  fig: list of expired configurations
      ::
      fig=(list config)
      ::  fal: list of failed order attempts
      ::
      fal=(list order)
  ==
::  +directory: ACME v2 service directory
::
+$  directory
  $:  ::  register: registration url (newAccount)
      ::
      register=purl
      ::  nonce: nonce creation url (newNonce)
      ::
      nonce=purl
      ::  new-order: order creation url (newOrder)
      ::
      new-order=purl
      ::  revoke: certificate revocation url (revokeCert)
      ::
      revoke=purl
      ::  rekey: account key revocation url (keyChange)
      ::
      rekey=purl
  ==
::  +acme: complete app state
::
+$  acme
  $:  ::  dir: ACME service directory
      ::
      dir=directory
      ::  act: ACME service account
      ::
      act=acct
      ::  liv: active, live configuration
      ::
      liv=(unit config)
      ::  hit: ACME account history
      ::
      hit=history
      ::  nonces: list of unused nonces
      ::
      nonces=(list @t)
      ::  rod: active, in-progress order
      ::
      rod=(unit order)
      ::  next-order: queued domains for validation
      ::
      next-order=(unit [try=@ud dom=(map turf [idx=@ud valid=?])])
      ::  cey: certificate key XX move?
      ::
      cey=key:rsa
      ::  challenges: domain-validation challenge tokens
      ::
      challenges=(set @t)
  ==
--
=|  acme
=*  state  -
=<
  %+  verb  |
  |_  =bowl:gall
  +*  this       .
      acme-core  +>
      ac         ~(. acme-core bowl)
      def        ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    =/  =binding:eyre
      [~ /'.well-known'/acme-challenge]
    =/  =generator:eyre
      [q.byk.bowl /gen/acme/domain-validation/hoon ~]
    =/  =card
      [%pass /acme %arvo %e %serve binding generator]
    [[card ~] this]
  ::
  ++  on-save   !>(state)
  ++  on-load   |=(old=vase `this(state !<(acme old)))
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        %acme-order  (poke-acme-order:ac !<((set turf) vase))
        %noun        (poke-noun:ac !<(* vase))
        %path        (poke-path:ac !<(path vase))
      ==
    [cards this]
  ::
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-peek
    |=  =path
    ^-  (unit (unit [%noun vase]))
    ?+    path  ~
        [%x %domain-validation @t ~]
      =*  token  i.t.t.path
      :^  ~  ~  %noun  !>
      ?.  (~(has in challenges) token)
        ~
      (some (rap 3 [token '.' (pass:thumb:jwk key.act) ~]))
    ==
  ::
  ++  on-agent  on-agent:def
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      ?+    wire  (on-arvo:def wire sign-arvo)
          [%acme *]
        ?+  +<.sign-arvo  (on-arvo:def wire sign-arvo)
          %http-response  (http-response:ac wire +>.sign-arvo)
          %wake           (wake:ac wire +>.sign-arvo)
          %bound          (bound:ac wire +>.sign-arvo)
        ==
      ==
    [cards this]
  ::
  ++  on-fail   on-fail:def
  --
::
::::  acme app
::
::  directory-base: LetsEncrypt service directory url
::
=/  directory-base=purl
  =-  (need (de-purl:html -))
  'https://acme-v02.api.letsencrypt.org/directory'
::  cards: list of outgoing moves for the current transaction
::
=|  cards=(list card)
::
|_  bow=bowl:gall
::  +this: self
::
++  this  .
::  +emit: emit a card
::
++  emit
  |=  car=card
  this(cards [car cards])
::  +emil: emit a list of cards
::
++  emil
  |=  rac=(list card)
  |-  ^+  this
  ?~  rac
    this
  =.  cards  [i.rac cards]
  $(rac t.rac)
::  +abet: finalize transaction
::
++  abet
  ^-  (quip card _state)
  [(flop cards) state]
::  +backoff: calculate exponential backoff
::
++  backoff
  |=  try=@ud
  ^-  @dr
  ?:  =(0 try)  ~s0
  %+  add
    (mul ~s1 (bex (dec try)))
  (mul ~s0..0001 (~(rad og eny.bow) 1.000))
::  +acme-wire: create :acme http-request wire
::
++  acme-wire
  |=  [try=@ud act=@tas =wire]
  ^-  ^wire
  (weld /acme/try/(scot %ud try)/[act] wire)
::  +notify: send notification message
::
++  notify
  |=  [=cord =tang]
  ^-  (list card)
  :-  [%pass / %arvo %d %flog %text :(weld (trip dap.bow) ": " (trip cord))]
  %+  turn
    `wall`(zing (turn (flop tang) (cury wash [0 80])))
  |=(=tape [%pass / %arvo %d %flog %text tape])
::  +request: unauthenticated http request
::
++  request
  |=  [wir=wire req=hiss]
  ^-  card
  [%pass wir %arvo %i %request (hiss-to-request:html req) *outbound-config:iris]
::  +signed-request: JWS JSON POST
::
++  signed-request
  |=  [url=purl non=@t bod=json]
  ^-  hiss
  :^  url  %post
    (my content-type+['application/jose+json' ~] ~)
  :-  ~
  ^-  octs
  =;  pro=json
    (as-octt:mimes:html (en-json:html (sign:jws key.act pro bod)))
  :-  %o  %-  my  :~
    nonce+s+non
    url+s+(crip (en-purl:html url))
    ?^  reg.act
      kid+s+kid.u.reg.act
    jwk+(pass:en:jwk key.act)
  ==
::  +stateful-request: emit signed, nonce'd request
::
++  stateful-request
  |=  [[try=@ud act=@tas =wire] =purl =json]
  ^+  this
  ?~  nonces
    (nonce:effect [act wire])
  %-  emit(nonces t.nonces)
  %+  request  (acme-wire try act wire)
  (signed-request purl i.nonces json)
::  +bad-nonce: check if an http response is a badNonce error
::
++  bad-nonce
  |=  rep=httr
  ^-  ?
  ::  XX always 400?
  ::
  ?.  =(400 p.rep)  |
  ?~  r.rep  |
  =/  jon=(unit json)  (de-json:html q.u.r.rep)
  ?~  jon  |
  =('urn:ietf:params:acme:error:badNonce' type:(error:grab u.jon))
::  +rate-limited: handle Acme service rate-limits
::
++  rate-limited
  |=  [try=@ud act=@tas spur=wire bod=(unit octs)]
  ^+  this
  =/  jon=(unit json)
    ?~(bod ~ (de-json:html q.u.bod))
  ?~  jon
    ::  no details, back way off
    ::  XX specifically based on wire
    ::
    (retry:effect try act spur (min ~d1 (backoff (add 10 try))))
  =/  err  (error:grab u.jon)
  ?.  =('params:acme:error:rateLimited' type.err)
    ::  incorrect 429 status? backoff normally
    ::
    (retry:effect try act spur (min ~h1 (backoff try)))

  =/  detail  (trip detail.err)
  ::  too many certificates for these domains
  ::
  ?:  ?=(^ (find "already issued for exact" detail))
    =.  ..emit  (retry:effect try act spur ~d7)
    =/  msg=cord
      %+  rap  3
      :~  'rate limit exceeded: '
          ' too many certificates issued for '
          ?~  rod
            ::  XX shouldn't happen
            ::
            (en-turf:html /network/arvo/(crip +:(scow %p our.bow)))
          (join-turf ~(tap in dom.u.rod))
          '. retrying in ~d7.'
      ==
    (emil (notify msg ~))
  ::  too many certificates for top-level-domain
  ::
  ?:  ?=(^ (find "too many certificates already" detail))
    =.  ..emit  (retry:effect try act spur ~d7)
    =/  lul=@dr
      (add ~d7 (mul ~m1 (~(rad og eny.bow) (bex 10))))
    =/  msg=cord
      %+  rap  3
      :~  'rate limit exceeded: '
          ' too many certificates issued for '
          ::  XX get from detail
          ::
          (en-turf:html /network/arvo)
          '. retrying in '
          (scot %dr lul)  '.'
      ==
    (emil (notify msg ~))
  ::  XX match more rate-limit conditions
  ::  or backoff by wire
  ::
  ::    - "too many registrations for this IP"
  ::    - "too many registrations for this IP range"
  ::    - "too many currently pending authorizations"
  ::    - "too many failed authorizations recently"
  ::    - "too many new orders recently"
  ::
  (retry:effect try act spur (min ~d1 (backoff (add 10 try))))
::  +failure-message: generic http failure message
::
++  failure-message
  |=  =purl
  ^-  cord
  %+  rap  3
  :~  'unable to reach '
      (crip (en-purl:html purl))  '. '
      'please confirm your urbit has network connectivity.'
  ==
::  |effect: send moves to advance
::
++  effect
  |_  try-count=(unit @ud)
  ::  +try: this effect attempt number
  ::
  ++  try  (fall try-count 1)
  ::  +validate-domain: confirm that a pending domain resolves to us
  ::
  ++  validate-domain
    |=  idx=@ud
    ^+  this
    ~|  %validate-domain-effect-fail
    ?.  ?=(^ next-order)  ~|(%no-next-order !!)
    =/  pending
      (skip ~(tap by dom.u.next-order) |=([turf @ud valid=?] valid))
    ?:  =(~ pending)
      new-order:effect
    =/  next=[=turf idx=@ud valid=?]
      ~|  [%no-next-domain idx=idx]
      (head (skim pending |=([turf idx=@ud ?] =(idx ^idx))))
    ::  XX should confirm that :turf points to us
    ::  confirms that domain exists (and an urbit is on :80)
    ::
    =/  =purl
        :-  [sec=| por=~ host=[%& turf.next]]
        [[ext=`~.udon path=/static] query=~]
    =/  =wire
      (acme-wire try %validate-domain /idx/(scot %ud idx.next))
    (emit (request wire purl %get ~ ~))
  ::  +directory: get ACME service directory
  ::
  ++  directory
    ^+  this
    ::  XX now in wire?
    ::
    (emit (request (acme-wire try %directory /) directory-base %get ~ ~))
  ::  +nonce: get a new nonce for the next request
  ::
  ++  nonce
    |=  nex=wire
    ~|  [%bad-nonce-next nex]
    ?>  ?&  ?=(^ nex)
            ?=(nonce-next i.nex)
        ==
    ^+  this
    ::  XX now in wire?
    ::
    =/  =wire
       (acme-wire try %nonce [%next nex])
    (emit (request wire nonce.dir %get ~ ~))
  ::  +register: create ACME service account
  ::
  ::    Note: accepts services ToS.
  ::    XX add rekey mechanism
  ::
  ++  register
    ^+  this
    ?.  =(~ reg.act)
      ?:  =(~ next-order)
        this
      (validate-domain:effect 0)
    =/  =json  [%o (my [['termsOfServiceAgreed' b+&] ~])]
    ::  XX date in wire?
    ::
    =/  wire-params  [try %register /]
    (stateful-request wire-params register.dir json)
  ::  +renew: renew certificate
  ::
  ++  renew
    ^+  this
    ~|  %renew-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ liv)      ~|(%no-live-config !!)
    =<  new-order:effect
    (queue-next-order 1 & dom.u.liv)
  ::  +new-order: create a new certificate order
  ::
  ++  new-order
    ^+  this
    ~|  %new-order-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=([~ ^] next-order)  ~|(%no-domains !!)
    =/  =json
      :-  %o  %-  my  :~
        :-  %identifiers
        :-  %a
        %+  turn
          ~(tap in ~(key by `(map turf *)`dom.u.next-order))
        |=(a=turf [%o (my type+s+'dns' value+s+(en-turf:html a) ~)])
      ==
    =/  wire-params  [try %new-order /(scot %da now.bow)]
    (stateful-request wire-params new-order.dir json)
  ::  +cancel-order: cancel failed order, set retry timer
  ::
  ++  cancel-order
    ^+  this
    ~|  %cancel-order-effect-fail
    =*  order  ?>(?=(^ rod) u.rod)  ::  XX TMI
    ::  backoff faster than usual
    ::
    =/  lul=@dr  (min ~h1 (backoff (add 5 try.order)))
    ::  XX get failure reason
    ::
    =/  msg=cord
      (cat 3 'retrying certificate request in ' (scot %dr lul))
    =.  ..emit  (emil (notify msg ~))
    =.  ..emit  (retry:effect try %new-order / lul)
    ::  domains might already be validated
    ::
    =.  ..emit  (queue-next-order +(try.order) & dom.order)
    cancel-current-order
  ::  +finalize-order: finalize completed order
  ::
  ++  finalize-order
    ^+  this
    ~|  %finalize-order-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    ?.  ?=(~ pending.aut.u.rod)  ~|(%pending-authz !!)
    ?.  ?=(~ active.aut.u.rod)   ~|(%active-authz !!)
    ::  XX revisit wrt rate limits
    ::
    ?>  ?=(%wake sas.u.rod)
    =/  =json
      [%o (my csr+s+(en-base64url (met 3 csr.u.rod) `@`csr.u.rod) ~)]
    =/  wire-params  [try %finalize-order /(scot %da now.bow)]
    (stateful-request wire-params fin.u.rod json)
  ::  +check-order: check completed order for certificate availability
  ::
  ++  check-order
    ^+  this
    ~|  %check-order-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    ?.  ?=(~ pending.aut.u.rod)  ~|(%pending-authz !!)
    ?.  ?=(~ active.aut.u.rod)   ~|(%active-authz !!)
    ::  XX revisit wrt rate limits
    ::
    ?>  ?=(%wake sas.u.rod)
    =/  =wire
       (acme-wire try %check-order /(scot %da now.bow))
    (emit (request wire ego.u.rod %get ~ ~))
  ::  +certificate: download PEM-encoded certificate
  ::
  ++  certificate
    |=  url=purl
    ^+  this
    ~|  %certificate-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    =/  hed  (my accept+['applicate/x-pem-file' ~] ~)
    =/  =wire
       (acme-wire try %certificate /(scot %da now.bow))
    (emit (request wire url %get hed ~))
  ::  +install: tell %eyre about our certificate
  ::
  ++  install
    ^+  this
    ~|  %install-effect-fail
    ?>  ?=(^ liv)
    =/  key=wain  (ring:en:pem:pkcs8 key.u.liv)
    (emit %pass /install %arvo %e %rule %cert `[key `wain`cer.u.liv])
  ::  +get-authz: get next ACME service domain authorization object
  ::
  ++  get-authz
    ^+  this
    ~|  %get-authz-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    ?.  ?=(^ pending.aut.u.rod)  ~|(%no-pending-authz !!)
    ::  XX revisit wrt rate limits
    ::
    ?>  ?=(%wake sas.u.rod)
    =/  =wire
      (acme-wire try %get-authz /(scot %da now.bow))
    (emit (request wire i.pending.aut.u.rod %get ~ ~))
  ::  XX check/finalize-authz ??
  ::
  ::  +test-trial: confirm that ACME domain validation challenge is available
  ::
  ++  test-trial
    ^+  this
    ~|  %test-trial-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    ?.  ?=(^ active.aut.u.rod)  ~|(%no-active-authz !!)
    ::  XX revisit wrt rate limits
    ::
    ?>  ?=(%wake sas.u.rod)
    =*  aut  u.active.aut.u.rod
    =/  pat=path  /'.well-known'/acme-challenge/[tok.cal.aut]
    ::  note: requires port 80, just as the ACME service will
    ::
    =/  url=purl  [[sec=| por=~ hos=[%& dom.aut]] [ext=~ pat] hed=~]
    ::  =/  url=purl  [[sec=| por=`8.081 hos=[%& /localhost]] [ext=~ pat] hed=~]
    ::  XX idx in wire?
    ::
    =/  =wire
      (acme-wire try %test-trial /(scot %da now.bow))
    (emit (request wire url %get ~ ~))
  ::  +finalize-trial: notify ACME service that challenge is ready
  ::
  ++  finalize-trial
    ^+  this
    ~|  %finalize-trial-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    ?.  ?=(^ active.aut.u.rod)  ~|(%no-active-authz !!)
    ::  XX revisit wrt rate limits
    ::
    ?>  ?=(%wake sas.u.rod)
    =*  aut  u.active.aut.u.rod
    ::  empty object included for signature
    ::  XX include index in wire?
    ::
    =/  wire-params  [try %finalize-trial /(scot %da now.bow)]
    (stateful-request wire-params ego.cal.aut [%o ~])
  ::  XX delete-trial?
  ::
  ::  +retry: retry effect after timeout
  ::
  ++  retry
    |=  [try=@ud act=@tas =wire lull=@dr]
    ::  XX validate wire
    ::
    (emit %pass (acme-wire +(try) act wire) %arvo %b %wait (add now.bow lull))
  --
::  |event: accept event, emit next effect(s)
::
::    XX should these next effects be triggered at call sites instead?
::
++  event
  |_  try=@ud
  ::  +validate-domain: accept a pending domain confirmation response
  ::
  ++  validate-domain
    |=  [=wire rep=httr]
    ^+  this
    ?>  ?=([%idx @ *] wire)
    ?.  ?=(^ next-order)
      this
    =/  idx  (slav %ud i.t.wire)
    =/  valid  |(=(200 p.rep) =(307 p.rep))
    =/  item=(list [=turf idx=@ud valid=?])
      (skim ~(tap by dom.u.next-order) |=([turf idx=@ud ?] =(^idx idx)))
    ?.  ?&  ?=([^ ~] item)
            !valid.i.item
        ==
      this
    =.  dom.u.next-order
      (~(put by dom.u.next-order) turf.i.item [idx valid])
    ?.  valid
      ?:  (lth try 10)
        =/  lul=@dr  (min ~h1 (backoff try))
        (retry:effect try %validate-domain /idx/(scot %ud idx) lul)
      ::  XX remove next-order, cancel pending requests
      ::  XX include suggestion to fix
      ::
      =/  msg=cord
        %+  rap  3
        :~  'unable to reach '  (scot %p our.bow)
            ' via http at '  (en-turf:html turf.i.item)  ':80'
        ==
      (emil(next-order ~) (notify msg [(sell !>(rep)) ~]))
    ?:  ?=(~ (skip ~(val by dom.u.next-order) |=([@ud valid=?] valid)))
      new-order:effect
    (validate-domain:effect +(idx))
  ::  +directory: accept ACME service directory, trigger registration
  ::
  ++  directory
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  =(200 p.rep)
      ?:  (lth try 10)
        (retry:effect try %directory / (min ~m30 (backoff try)))
      (emil (notify (failure-message directory-base) [(sell !>(rep)) ~]))
    =.  dir  (directory:grab (need (de-json:html q:(need r.rep))))
    ?~(reg.act register:effect this)
  ::  +nonce: accept new nonce and trigger next effect
  ::
  ::    Nonce has already been saved in +http-response. The next effect
  ::    is specified in the wire.
  ::
  ++  nonce
    |=  [=wire rep=httr]
    ^+  this
    ~|  [%unrecognized-nonce-wire wire]
    ?>  &(?=(^ wire) ?=([%next ^] wire))
    =*  nex  i.t.wire
    ~|  [%unknown-nonce-next nex]
    ?>  ?=(nonce-next nex)
    ?.  =(204 p.rep)
      ?:  (lth try 10)
        (retry:effect try %nonce t.wire (min ~m30 (backoff try)))
      (emil (notify (failure-message nonce.dir) [(sell !>(rep)) ~]))
    ?-  nex
      %register        register:effect
      %new-order       new-order:effect
      %finalize-order  finalize-order:effect
      %finalize-trial  finalize-trial:effect
    ==
  ::  +register: accept ACME service registration
  ::
  ++  register
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  |(=(200 p.rep) =(201 p.rep))
      ::  XX possible 204?
      ::
      ?:  (lth try 10)
        (retry:effect try %register / (min ~h1 (backoff try)))
      (emil (notify (failure-message register.dir) [(sell !>(rep)) ~]))
    =/  loc=@t
      q:(head (skim q.rep |=((pair @t @t) ?=(%location p))))
    ::  XX @da once parser is fixed
    ::
    =/  wen=@t
      ?~  r.rep
        (scot %da now.bow)
      =/  bod=acct:body
        (acct:grab (need (de-json:html q.u.r.rep)))
      ?>  ?=(%valid sas.bod)
      wen.bod
    =.  reg.act  `[wen loc]
    ?:  =(~ next-order)
      this
    (validate-domain:effect 0)
  ::  XX rekey
  ::
  ::  +new-order: order created, begin processing authorizations
  ::
  ++  new-order
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  =(201 p.rep)
      ::  XX possible 204?
      ::
      ?:  (lth try 10)
        (retry:effect try %new-order / (min ~h1 (backoff try)))
      ::  XX next steps, retrying in ??
      ::
      (emil (notify (failure-message register.dir) [(sell !>(rep)) ~]))
    ?>  ?=(^ next-order)
    =/  loc=@t
      q:(head (skim q.rep |=((pair @t @t) ?=(%location p))))
    =/  ego=purl  (need (de-purl:html loc))
    ::  XX parse identifiers, confirm equal to pending domains
    ::  XX check status
    ::
    =/  bod=order:body
      (order:grab (need (de-json:html q:(need r.rep))))
    =/  dom=(set turf)  ~(key by dom.u.next-order)
    ::  XX maybe generate key here?
    ::
    =/  csr=@ux  +:(en:der:pkcs10 cey ~(tap in dom))
    =/  dor=order
      :*  dom
          try.u.next-order
          sas=%wake
          exp.bod
          ego
          (need fin.bod)
          cey
          csr
          [aut.bod ~ ~]
      ==
    get-authz:effect(rod `dor, next-order ~)
  ::  +finalize-order: order finalized, poll for certificate
  ::
  ++  finalize-order
    |=  [wir=wire rep=httr]
    ^+  this
    ?:  =(504 p.rep)
      ::  retry timeouts frequently
      ::
      (retry:effect try %finalize-order / (min ~m10 (backoff try)))
    ::  check-order regardless of status code
    ::
    check-order:effect
  ::  +check-order: check order status, dispatch appropriately
  ::
  ++  check-order
    |=  [wir=wire rep=httr]
    ^+  this
    ~|  [%strange-check-order wir]
    ?>  ?=(^ rod)
    ?.  =(200 p.rep)
      ?:  (lth try 10)
        (retry:effect try %check-order / (min ~m10 (backoff try)))
      ::  XX next steps, retrying in, delete order ??
      ::
      (emil (notify (failure-message ego.u.rod) [(sell !>(rep)) ~]))
    =/  bod=order:body
      (order:grab (need (de-json:html q:(need r.rep))))
    ?+  sas.bod
      ~&  [%check-order-status-unknown sas.bod]
      this
    ::  order failed (at any stage)
    ::
        %invalid
      ~&  [%check-order-fail %invalid wir rep]
      ::  XX check authz, get the failure reason
      ::  XX possible to retry any reasons?
      ::
      =<  cancel-order:effect
      (emil (notify 'certificate order failed' [(sell !>(rep)) ~]))
    ::  initial order state
    ::
        %pending
      check-order:effect
    ::  validations completed
    ::
        %ready
      finalize-order:effect
    ::  finalization requested
    ::
        %processing
      check-order:effect
    ::  certificate issued
    ::
        %valid
      ::  XX update order state
      ::  XX =< delete-trial
      ::
      ~|  impossible-order+[wir rep bod]
      (certificate:effect (need cer.bod))
    ==
  ::
  ::  +certificate: accept PEM-encoded certificate
  ::
  ++  certificate
    |=  [wir=wire rep=httr]
    ^+  this
    ~|  [%strange-certificate-response wir]
    ?>  ?=(^ rod)
    ?.  =(200 p.rep)
      ::  will re-attempt certificate download per order status
      ::
      ?:  (lth try 10)
        (retry:effect try %check-order / (min ~m10 (backoff try)))
      ::  XX next steps, retrying in, get url somehow ??
      ::
      =/  msg=cord
        %+  rap  3
        :~  'unable to download certificate. '
            'please confirm that your urbit has network connectivity.'
        ==
      (emil (notify msg [(sell !>(rep)) ~]))
    =/  cer=wain  (to-wain:format q:(need r.rep))
    =/  fig=config
      ::  XX expiration date
      ::
      [dom.u.rod key.u.rod cer (add now.bow ~d90) ego.u.rod]
    ::  archive live config
    ::
    =?  fig.hit  ?=(^ liv)  [u.liv fig.hit]
    ::  save new live config, clear active order
    ::
    =>  .(liv (some fig), rod ~)
    ?>  ?=(^ liv)
    ::  notify :hall
    ::
    =>  =/  msg=cord
          %+  rap  3
          :~  'received https certificate for '
              (join-turf ~(tap in dom.u.liv))
          ==
        (emil (notify msg ~))
    ::  set renewal timer, install certificate in %eyre
    ::
    ::    Certificates expire after ~d90. We want time for retries and
    ::    to work around rate limits, so our renewal timer is for ~d60.
    ::    Renewals count towards weekly rate limits, but are allowed to
    ::    continue past rate limits, so fudge the timer towards the end
    ::    of the week nearest ~d60.
    ::
    =<  install:effect
    =;  lul=@dr
      (retry:effect 0 %renew / lul)
    %+  add
      (mul ~m1 (~(rad og eny.bow) (bex 8)))
    =/  weekday  (daws:chrono:userlib (yore now.bow))
    ?:  (gth weekday 4)
      (sub ~d60 (mul ~d1 (sub weekday 4)))
    (add ~d60 (mul ~d1 (sub 4 weekday)))
  ::  +get-authz: accept ACME service authorization object
  ::
  ++  get-authz
    |=  [wir=wire rep=httr]
    ^+  this
    ~|  [%strange-authorization wir]
    ?>  ?=(^ rod)
    ?>  ?=(^ pending.aut.u.rod)
    ?.  =(200 p.rep)
      ?:  (lth try 10)
        (retry:effect try %get-authz / (min ~m10 (backoff try)))
      ::  XX next steps, retrying in ??
      ::
      (emil (notify (failure-message i.pending.aut.u.rod) [(sell !>(rep)) ~]))
    =/  bod=auth:body
      (auth:grab (need (de-json:html q:(need r.rep))))
    =/  cal=trial
       ::  XX parse token to verify url-safe base64?
       ::
      [%http url.cal.bod tok.cal.bod %recv]
    ::  XX check that URLs are the same
    ::
   =/  tau=auth  [i.pending.aut.u.rod dom.bod cal]
    ::  XX get idx from wire instead?
    ::
    =/  idx=@ud  +((lent done.aut.u.rod))
    =/  rod-aut=order-auth
      %=  aut.u.rod
        pending  t.pending.aut.u.rod
        active   `[idx tau]
      ==
    ::  XX space leak, should be pruned on order completion or timeout
    ::
    =.  challenges  (~(put in challenges) tok.cal)
    test-trial:effect(aut.u.rod rod-aut)
  ::  XX check/finalize-authz ??
  ::
  ::  +test-trial: accept response from challenge test
  ::
  ++  test-trial
    |=  [wir=wire rep=httr]
    ~|  [%strange-test-trial wir]
    ?>  ?=(^ rod)
    ?>  ?=(^ active.aut.u.rod)
    =*  aut  u.active.aut.u.rod
    ^+  this
    ?.  =(200 p.rep)
      ?:  (lth try 10)
        (retry:effect try %test-trial / (min ~m10 (backoff try)))
      ::  XX next steps, check connectivity, etc. ??
      ::
      =<  cancel-order:effect
      =/  msg=cord
        %+  rap  3
        :~  'unable to retrieve self-hosted domain validation token '
            'via '  (en-turf:html dom.aut)  '. '
            'please confirm your urbit has network connectivity.'
        ==
      (emil (notify msg [(sell !>(rep)) ~]))
    =/  bod
      %-  as-octs:mimes:html
      (rap 3 [tok.cal.aut '.' (pass:thumb:jwk key.act) ~])
    ?.  ?&  ?=(^ r.rep)
            =(bod u.r.rep)
        ==
      ::  XX probably a DNS misconfiguration
      ::
      =/  =tang
        :~  ?~(r.rep leaf+"~" (sell !>(u.r.rep)))
            leaf+"actual:"
            (sell !>((some bod)))
            leaf+"expected:"
        ==
      (emil (notify 'domain validation value is wrong' tang))
    finalize-trial:effect
  ::  +finalize-trial:
  ::
  ++  finalize-trial
    |=  [wir=wire rep=httr]
    ^+  this
    ~|  [%strange-finalize-trial wir]
    ?>  ?=(^ rod)
    ?>  ?=(^ active.aut.u.rod)
    =*  aut  u.active.aut.u.rod
    ?.  =(200 p.rep)
      ::  XX possible 204? assume pending?
      ::  XX handle "challenge is not pending"
      ::
      ?:  =(504 p.rep)
        ::  retry timeouts frequently
        ::
        ?:  (lth try 10)
          (retry:effect try %finalize-trial / (min ~m10 (backoff try)))
        ::  XX next steps, check connectivity, etc. ??
        ::
        (emil (notify (failure-message ego.cal.aut) [(sell !>(rep)) ~]))
      ::  XX get challenge, confirm urn:ietf:params:acme:error:connection
      ::
      ::  =/  err=error:body
      ::    (error:grab (need (de-json:html q:(need r.rep))))
      ::  ?:  =('urn:ietf:params:acme:error:malformed' status.err)
      ::
      =<  cancel-order:effect
      =/  msg=cord
       'unable to finalize domain validation challenge'
      (emil (notify msg [(sell !>(rep)) ~]))
    =/  bod=challenge:body
      (challenge:grab (need (de-json:html q:(need r.rep))))
    ::  XX check for other possible values in 200 response
    ::  note: may have already been validated
    ::
    ?>  ?=(?(%pending %valid) sas.bod)
    =/  rod-aut=order-auth
      aut.u.rod(active ~, done [+.aut(sas.cal %pend) done.aut.u.rod])
    ?~  pending.aut.u.rod
      check-order:effect(aut.u.rod rod-aut)
    get-authz:effect(aut.u.rod rod-aut)
  ::  XX delete-trial?
  ::
  ::  +retry: retry effect after timeout
  ::
  ++  retry
    |=  =wire
    ^+  this
    ?>  ?=([%try @ @tas *] wire)
    =/  try  (slav %ud i.t.wire)
    =*  fec  ~(. effect (some +(try)))
    =*  act  i.t.t.wire
    =*  spur  t.t.t.wire
    ?+  act
        ~&([%unknown-retry act] this)
      %validate-domain
                       ?>  ?=([%idx @ ~] spur)
                       (validate-domain:fec (slav %ud i.t.spur))
      %directory       directory:fec
      %nonce           ?>  ?=(^ spur)
                       (nonce:fec t.spur)
      %register        register:fec
      %renew           renew:fec
      %new-order       new-order:fec
      %finalize-order  finalize-order:fec
      %check-order     check-order:fec
      %certificate     check-order:fec :: intentional
      %get-authz       get-authz:fec
      %test-trial      test-trial:fec
      %finalize-trial  finalize-trial:fec
    ==
  --
++  http-response
  |=  [=wire response=client-response:iris]
  ^-  (quip card _state)
  ::  ignore progress reports
  ::
  ?:  ?=(%progress -.response)
    [~ state]
  ::
  ?>  ?=([%acme ^] wire)
  =<  abet
  ::
  ?:  ?=(%cancel -.response)
    (retry:event t.wire)
  ::
  =/  rep=httr  (to-httr:iris +.response)
  ::  add nonce to pool, if present
  ::
  =/  nonhed  (skim q.rep |=((pair @t @t) ?=(%replay-nonce p)))
  =?  nonces  ?=(^ nonhed)  [q.i.nonhed nonces]
  ::
  ?>  ?=([%try @ @tas *] t.wire)
  =/  try  (slav %ud i.t.t.wire)
  =*  ven  ~(. event try)
  =*  act  i.t.t.t.wire
  =*  spur  t.t.t.t.wire
  ::  backoff if rate-limited
  ::
  ?:  =(429 p.rep)
    (rate-limited try act spur r.rep)
  ::  request nonce if expired-invalid
  ::
  ?:  (bad-nonce rep)
    (nonce:effect [act spur])
  ::  XX replace with :hall notification
  ::
  ~|  [%http-response-fail wire]
  %.  [spur rep]
  ?+  act
      ~&([%unknown-http-response act] !!)
    %validate-domain
                     validate-domain:ven
    %directory       directory:ven
    %nonce           nonce:ven
    %register        register:ven
    ::  XX rekey
    ::
    %new-order       new-order:ven
    %finalize-order  finalize-order:ven
    %check-order     check-order:ven
    %certificate     certificate:ven
    %get-authz       get-authz:ven
    ::  XX check/finalize-authz ??
    ::
    %test-trial      test-trial:ven
    %finalize-trial  finalize-trial:ven
    ::  XX delete-trial?
    ::
  ==
::  +wake: timer wakeup event
::
++  wake
  |=  [wir=wire error=(unit tang)]
  ^-  (quip card _state)
  ?^  error
    %-  (slog u.error)
    abet
  ?>  ?=([%acme *] wir)
  abet:(retry:event t.wir)
::  +poke-acme-order: create new order for a set of domains
::
++  poke-acme-order
  |=  a=(set turf)
  abet:(add-order a)
::  +poke-noun: for debugging
::
++  poke-noun
  |=  a=*
  ^-  (quip card _state)
  =<  abet
  ?+  a
    this
  ::
      %dbug-account
    ~&  registered=reg.act
    ~&  [%public (pass:en:pem:pkcs1 key.act)]
    ~?  !=(~ sek.key.act)
      [%private (ring:en:pem:pkcs1 key.act)]
    this
  ::
      %dbug-certificate
    ?~  liv  ~&(~ this)
    ~&  [%key (ring:en:pem:pkcs8 key.u.liv)]
    ~&  [%cert `wain`cer.u.liv]
    ~&  [%expires exp.u.liv]
    ~&  :-  %domains
        (join-turf ~(tap in dom.u.liv))
    this
  ::
      %dbug-history
    ~&  [%account-history act.hit]
    ~&  [%config-history fig.hit]
    ~&  [%failed-order-history fal.hit]
    this
  ::
    ::  install privkey and cert .pem from /=home=/acme, ignores app state
    ::TODO  refactor this out of %acme, see also arvo#1151
    ::
      %install-from-clay
    =/  bas=path  /(scot %p our.bow)/home/(scot %da now.bow)/acme
    =/  key=wain  .^(wain %cx (weld bas /privkey/pem))
    =/  cer=wain  .^(wain %cx (weld bas /cert/pem))
    (emit %pass /install %arvo %e %rule %cert `[key cer])
  ::
      %init
    init
  ::
      %register
    register:effect
  ::
      %poll
    check-order:effect
  ::
      %retry
    (add-order (sy /network/arvo/(crip +:(scow %p our.bow)) ~))
  ==
::  +poke-path: for debugging
::
++  poke-path
  |=(a=path abet:(add-order (sy a ~)))
::  +bound: response to %serve binding request
::
++  bound
  |=  [=wire accepted=? =binding:eyre]
  ?:  accepted
    [~ state]
  ::  XX better error message
  ::
  ~&  [%acme-http-path-binding-failed +<]
  [~ state]
::  +rekey: create new 2.048 bit RSA key
::
::    XX do something about this iteration
::
++  rekey
  |=  eny=@
  =|  i=@
  |-  ^-  key:rsa
  =/  k  (new-key:rsa 2.048 eny)
  =/  m  (met 0 n.pub.k)
  ::  ?:  =(0 (mod m 8))  k
  ?:  =(2.048 m)  k
  ~&  [%key iter=i width=m]
  $(i +(i), eny +(eny))
::  +init: initialize :acme state
::
::    We defer the initial request for independence from the causal event,
::    which is necessary to init on the boot event. Which we no longer do,
::    but we're preserving the pattern for future flexibility.
::
++  init
  =<  (retry:effect 0 %directory / `@dr`1)
  %=  this
    act  [(rekey eny.bow) ~]
    cey  (rekey (mix eny.bow (shaz now.bow)))
  ==
::  +queue-next-order: enqueue domains for validation
::
++  queue-next-order
  |=  [try=@ud valid=? dom=(set turf)]
  ^+  this
  %=  this  next-order
    :+  ~
      try
    %+  roll
      ~(tap in dom)
    |=  [=turf state=(map turf [idx=@ud valid=?])]
    (~(put by state) turf [~(wyt by state) valid])
  ==
::  +cancel-current-order: and archive failure for future autopsy
::
::    XX we may have pending moves out for this order
::    put dates in wires, check against order creation date?
::    or re-use order-id?
::
++  cancel-current-order
  ^+  this
  ?~  rod  this
  %=  this
    rod      ~
    fal.hit  [u.rod fal.hit]
  ==
::  +add-order: add new certificate order
::
++  add-order
  |=  dom=(set turf)
  ^+  this
  ?:  =(~ dom)
    ~|(%acme-empty-certificate-order !!)
  ?:  ?=(?(%earl %pawn) (clan:title our.bow))
    this
  =.  ..emit  (queue-next-order 1 | dom)
  =.  ..emit  cancel-current-order
  ::  notify :hall
  ::
  =.  ..emit
    =/  msg=cord
      %+  rap  3
      :~  'requesting an https certificate for '
          (join-turf ~(tap in dom))
      ==
    (emil (notify msg ~))
  ::  if registered, create order
  ::
  ?^  reg.act
    (validate-domain:effect 0)
  ::  if initialized, defer
  ::
  ?.(=(act *acct) this init)
--
