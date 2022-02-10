!:
::  lighter than eyre
::
|=  our=ship
=,  eyre
::  internal data structures
::
=>  =~
::
::  internal data structures that won't go in zuse
::
|%
+$  move
  ::
  $:  ::  duct: request identifier
      ::
      =duct
      ::
      ::
      card=(wind note gift)
  ==
::  +note: private request from eyre to another vane
::
+$  note
  $%  ::  %b: to behn
      ::
      $:  %b
          ::
          ::
          $%  [%rest p=@da]
              [%wait p=@da]
      ==  ==
      $:  %c
          $>(%warp task:clay)
      ==
      ::  %d: to dill
      ::
      $:  %d
          ::
          ::
      $%  [%flog =flog:dill]
      ==  ==
      ::  %g: to gall
      ::
      $:  %g
          ::
          ::
          $>(%deal task:gall)
  ==  ==
::  +sign: private response from another vane to eyre
::
+$  sign
  $%  $:  %behn
          $%  [%wake error=(unit tang)]
      ==  ==
      $:  %gall
          gift:gall
          ::  $>(%unto gift:gall)
          ::
      ==
      $:  %clay
          gift:clay
          ::  $>(%writ gift:clay)
          ::
  ==  ==
--
::  more structures
::
|%
++  axle
  $:  ::  date: date at which http-server's state was updated to this data structure
      ::
      date=%~2020.10.18
      ::  server-state: state of inbound requests
      ::
      =server-state
  ==
::  +server-state: state relating to open inbound HTTP connections
::
+$  server-state
  $:  ::  bindings: actions to dispatch to when a binding matches
      ::
      ::    Eyre is responsible for keeping its bindings sorted so that it
      ::    will trigger on the most specific binding first. Eyre should send
      ::    back an error response if an already bound binding exists.
      ::
      ::    TODO: It would be nice if we had a path trie. We could decompose
      ::    the :binding into a (map (unit @t) (trie knot =action)).
      ::
      bindings=(list [=binding =duct =action])
      ::  cors-registry: state used and managed by the +cors core
      ::
      =cors-registry
      ::  connections: open http connections not fully complete
      ::
      connections=(map duct outstanding-connection)
      ::  authentication-state: state managed by the +authentication core
      ::
      =authentication-state
      ::  channel-state: state managed by the +channel core
      ::
      =channel-state
      ::  domains: domain-names that resolve to us
      ::
      domains=(set turf)
      ::  http-config: our server configuration
      ::
      =http-config
      ::  ports: live servers
      ::
      ports=[insecure=@ud secure=(unit @ud)]
      ::  outgoing-duct: to unix
      ::
      outgoing-duct=duct
  ==
::  channel-request: an action requested on a channel
::
+$  channel-request
  $%  ::  %ack: acknowledges that the client has received events up to :id
      ::
      [%ack event-id=@ud]
      ::  %poke: pokes an application, translating :json to :mark.
      ::
      [%poke request-id=@ud ship=@p app=term mark=@tas =json]
      ::  %watch: subscribes to an application path
      ::
      [%subscribe request-id=@ud ship=@p app=term =path]
      ::  %leave: unsubscribes from an application path
      ::
      [%unsubscribe request-id=@ud subscription-id=@ud]
      ::  %delete: kills a channel
      ::
      [%delete ~]
  ==
::  clog-timeout: the delay between acks after which clog-threshold kicks in
::
++  clog-timeout     ~s30
::  clog-threshold: maximum per-subscription event buildup, after clog-timeout
::
++  clog-threshold   50
::  channel-timeout: the delay before a channel should be reaped
::
++  channel-timeout  ~h12
::  session-timeout: the delay before an idle session expires
::
++  session-timeout  ~d7
--
::  utilities
::
|%
::  +combine-octs: combine multiple octs into one
::
++  combine-octs
  |=  a=(list octs)
  ^-  octs
  :-  %+  roll  a
      |=  [=octs sum=@ud]
      (add sum p.octs)
  (can 3 a)
::  +prune-events: removes all items from the front of the queue up to :id
::
::    also produces, per request-id, the amount of events that have got acked,
::    for use with +subtract-acked-events.
::
++  prune-events
  =|  acked=(map @ud @ud)
  |=  [q=(qeu [id=@ud @ud channel-event]) id=@ud]
  ^+  [acked q]
  ::  if the queue is now empty, that's fine
  ::
  ?:  =(~ q)
    [acked ~]
  ::
  =/  next=[item=[id=@ud request-id=@ud channel-event] _q]  ~(get to q)
  ::  if the head of the queue is newer than the acknowledged id, we're done
  ::
  ?:  (gth id.item.next id)
    [acked q]
  ::  otherwise, note the ack, and check next item
  ::
  %_  $
    q  +:next
  ::
      acked
    =,  item.next
    %+  ~(put by acked)  request-id
    +((~(gut by acked) request-id 0))
  ==
::  +subtract-acked-events: update the subscription map's pending ack counts
::
++  subtract-acked-events
  |=  [acked=(map @ud @ud) unacked=(map @ud @ud)]
  ^+  unacked
  %+  roll  ~(tap by acked)
  |=  [[rid=@ud ack=@ud] unacked=_unacked]
  ?~  sus=(~(get by unacked) rid)
    unacked
  %+  ~(put by unacked)  rid
  ?:  (lte u.sus ack)  0
  (sub u.sus ack)
::  +parse-channel-request: parses a list of channel-requests
::
::    Parses a json array into a list of +channel-request. If any of the items
::    in the list fail to parse, the entire thing fails so we can 400 properly
::    to the client.
::
++  parse-channel-request
  |=  request-list=json
  ^-  (unit (list channel-request))
  ::  parse top
  ::
  =,  dejs-soft:format
  =-  ((ar -) request-list)
  ::
  |=  item=json
  ^-  (unit channel-request)
  ::
  ?~  maybe-key=((ot action+so ~) item)
    ~
  ?:  =('ack' u.maybe-key)
    ((pe %ack (ot event-id+ni ~)) item)
  ?:  =('poke' u.maybe-key)
    ((pe %poke (ot id+ni ship+(su fed:ag) app+so mark+(su sym) json+some ~)) item)
  ?:  =('subscribe' u.maybe-key)
    %.  item
    %+  pe  %subscribe
    (ot id+ni ship+(su fed:ag) app+so path+(su stap) ~)
  ?:  =('unsubscribe' u.maybe-key)
    %.  item
    %+  pe  %unsubscribe
    (ot id+ni subscription+ni ~)
  ?:  =('delete' u.maybe-key)
    `[%delete ~]
  ::  if we reached this, we have an invalid action key. fail parsing.
  ::
  ~
::  +login-page: internal page to login to an Urbit
::
++  login-page
  |=  [redirect-url=(unit @t) our=@p failed=?]
  ^-  octs
  =+  redirect-str=?~(redirect-url "" (trip u.redirect-url))
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  =/  favicon  %+
    weld  "<svg width='10' height='10' viewBox='0 0 10 10' xmlns='http://www.w3.org/2000/svg'>"
          "<circle r='3.09' cx='5' cy='5' /></svg>"
  ;html
    ;head
      ;meta(charset "utf-8");
      ;meta(name "viewport", content "width=device-width, initial-scale=1, shrink-to-fit=no");
      ;link(rel "icon", type "image/svg+xml", href (weld "data:image/svg+xml;utf8," favicon));
      ;title:"OS1"
      ;style:'''
             @import url("https://rsms.me/inter/inter.css");
             @font-face {
                 font-family: "Source Code Pro";
                 src: url("https://storage.googleapis.com/media.urbit.org/fonts/scp-regular.woff");
                 font-weight: 400;
                 font-display: swap;
             }
             :root {
               --red05: rgba(255,65,54,0.05);
               --red100: rgba(255,65,54,1);
               --blue05: rgba(33,157,255,0.05);
               --blue30: rgba(33,157,255,0.3);
               --blue100: rgba(33,157,255,1);
               --black05: rgba(0,0,0,0.05);
               --black20: rgba(0,0,0,0.2);
               --black60: rgba(0,0,0,0.6);
               --white: rgba(255,255,255,1);
             }
             html {
               font-family: Inter, sans-serif;
               height: 100%;
               margin: 0;
               width: 100%;
               background: var(--white);
               color: var(--black100);
               -webkit-font-smoothing: antialiased;
               line-height: 1.5;
               font-size: 12px;
               display: flex;
               flex-flow: row nowrap;
               justify-content: center;
             }
             body {
               display: flex;
               flex-flow: column nowrap;
               justify-content: center;
               max-width: 300px;
               padding: 1rem;
               width: 100%;
             }
             body > *,
             form > input {
               width: 100%;
             }
             form {
               display: flex;
               flex-flow: column;
               align-items: flex-start;
             }
             input {
               background: transparent;
               border: 1px solid var(--black20);
               padding: 8px;
               border-radius: 4px;
               font-size: inherit;
               color: var(--black);
               box-shadow: none;
             }
             input:disabled {
               background: var(--black05);
               color: var(--black60);
             }
             input:focus {
               outline: none;
               border-color: var(--blue30);
             }
             input:invalid:not(:focus) {
               background: var(--red05);
               border-color: var(--red100);
               outline: none;
               color: var(--red100);
             }
             button[type=submit] {
               margin-top: 16px;
               padding: 8px 16px;
               border-radius: 4px;
               background: var(--blue100);
               color: var(--white);
               border: 1px solid var(--blue100);
             }
             input:invalid ~ button[type=submit] {
               border-color: currentColor;
               background: var(--blue05);
               color: var(--blue30);
               pointer-events: none;
             }
             span.failed {
               display: flex;
               flex-flow: row nowrap;
               height: 16px;
               align-items: center;
               margin-top: 6px;
               color: var(--red100);
             }
             span.failed svg {
               height: 12px;
              margin-right: 6px;
             }
             span.failed circle,
             span.failed line {
               fill: transparent;
               stroke: currentColor
             }
             .mono {
               font-family: 'Source Code Pro', monospace;
             }
             @media all and (prefers-color-scheme: dark) {
               :root {
                 --white: rgb(51, 51, 51);
                 --black100: rgba(255,255,255,1);
                 --black05: rgba(255,255,255,0.05);
                 --black20: rgba(255,255,255,0.2);
               }
             }
             '''
    ==
    ;body
      ;p:"Urbit ID"
      ;input(value "{(scow %p our)}", disabled "true", class "mono");
      ;p:"Access Key"
      ;form(action "/~/login", method "post", enctype "application/x-www-form-urlencoded")
        ;input
          =type  "password"
          =name  "password"
          =placeholder  "sampel-ticlyt-migfun-falmel"
          =class  "mono"
          =required  "true"
          =minlength  "27"
          =maxlength  "27"
          =pattern  "((?:[a-z]\{6}-)\{3}(?:[a-z]\{6}))"
          =autofocus  "true";
        ;input(type "hidden", name "redirect", value redirect-str);
        ;+  ?.  failed  ;span;
          ;span.failed
            ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 12 12")
              ;circle(cx "6", cy "6", r "5.5");
              ;line(x1 "3.27", y1 "3.27", x2 "8.73", y2 "8.73");
              ;line(x1 "8.73", y1 "3.27", x2 "3.27", y2 "8.73");
            ==
            Key is incorrect
          ==
          ;button(type "submit"):"Continue"
      ==
    ==
    ;script:'''
            var failSpan = document.querySelector('.failed');
            if (failSpan) {
              document.querySelector("input[type=password]")
                .addEventListener('keyup', function (event) {
                  failSpan.style.display = 'none';
                });
            }
            '''
  ==
::  +render-tang-to-marl: renders a tang and adds <br/> tags between each line
::
++  render-tang-to-marl
  |=  [wid=@u tan=tang]
  ^-  marl
  =/  raw=(list tape)  (zing (turn tan |=(a=tank (wash 0^wid a))))
  ::
  |-  ^-  marl
  ?~  raw  ~
  [;/(i.raw) ;br; $(raw t.raw)]
::  +render-tang-to-wall: renders tang as text lines
::
++  render-tang-to-wall
  |=  [wid=@u tan=tang]
  ^-  wall
  (zing (turn tan |=(a=tank (wash 0^wid a))))
::  +wall-to-octs: text to binary output
::
++  wall-to-octs
  |=  =wall
  ^-  (unit octs)
  ::
  ?:  =(~ wall)
    ~
  ::
  :-  ~
  %-  as-octs:mimes:html
  %-  crip
  %-  zing  ^-  ^wall
  %-  zing  ^-  (list ^wall)
  %+  turn  wall
  |=  t=tape
  ^-  ^wall
  ~[t "\0a"]
::  +internal-server-error: 500 page, with a tang
::
++  internal-server-error
  |=  [authorized=? url=@t t=tang]
  ^-  octs
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  ;html
    ;head
      ;title:"500 Internal Server Error"
    ==
    ;body
      ;h1:"Internal Server Error"
      ;p:"There was an error while handling the request for {(trip url)}."
      ;*  ?:  authorized
            ;=
              ;code:"*{(render-tang-to-marl 80 t)}"
            ==
          ~
    ==
  ==
::  +error-page: error page, with an error string if logged in
::
++  error-page
  |=  [code=@ud authorized=? url=@t t=tape]
  ^-  octs
  ::
  =/  code-as-tape=tape  (format-ud-as-integer code)
  =/  message=tape
    ?+  code  "{(scow %ud code)} Error"
      %400  "Bad Request"
      %403  "Forbidden"
      %404  "Not Found"
      %405  "Method Not Allowed"
      %500  "Internal Server Error"
    ==
  ::
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  ;html
    ;head
      ;title:"{code-as-tape} {message}"
    ==
    ;body
      ;h1:"{message}"
      ;p:"There was an error while handling the request for {(trip url)}."
      ;*  ?:  authorized
            ;=
              ;code:"{t}"
            ==
          ~
    ==
  ==
::  +format-ud-as-integer: prints a number for consumption outside urbit
::
++  format-ud-as-integer
  |=  a=@ud
  ^-  tape
  ?:  =(0 a)  ['0' ~]
  %-  flop
  |-  ^-  tape
  ?:(=(0 a) ~ [(add '0' (mod a 10)) $(a (div a 10))])
::  +host-matches: %.y if the site :binding should be used to handle :host
::
++  host-matches
  |=  [binding=(unit @t) host=(unit @t)]
  ^-  ?
  ::  if the binding allows for matching anything, match
  ::
  ?~  binding
    %.y
  ::  if the host is ~, that means we're trying to bind nothing to a real
  ::  binding. fail.
  ::
  ?~  host
    %.n
  ::  otherwise, do a straight comparison
  ::
  =(u.binding u.host)
::  +find-suffix: returns [~ /tail] if :full is (weld :prefix /tail)
::
++  find-suffix
  |=  [prefix=path full=path]
  ^-  (unit path)
  ?~  prefix
    `full
  ?~  full
    ~
  ?.  =(i.prefix i.full)
    ~
  $(prefix t.prefix, full t.full)
::  +simplified-url-parser: returns [(each @if @t) (unit port=@ud)]
::
++  simplified-url-parser
  ;~  plug
    ;~  pose
      %+  stag  %ip
      =+  tod=(ape:ag ted:ab)
      %+  bass  256
      ;~(plug tod (stun [3 3] ;~(pfix dot tod)))
    ::
      (stag %site (cook crip (star ;~(pose dot alp))))
    ==
    ;~  pose
      (stag ~ ;~(pfix col dim:ag))
      (easy ~)
    ==
  ==
::  +per-server-event: per-event server core
::
++  per-server-event
  ~%  %eyre-per-server-event  ..part  ~
  ::  gate that produces the +per-server-event core from event information
  ::
  |=  [[eny=@ =duct now=@da rof=roof] state=server-state]
  =/  eyre-id  (scot %ta (cat 3 'eyre_' (scot %uv (sham duct))))
  |%
  ::  +request-local: bypass authentication for local lens connections
  ::
  ++  request-local
    |=  [secure=? =address =request:http]
    ^-  [(list move) server-state]
    ::
    =/  act  [%app app=%lens]
    ::
    =/  connection=outstanding-connection
      [act [& secure address request] ~ 0]
    ::
    =.  connections.state
      (~(put by connections.state) duct connection)
    ::
    :_  state
    (subscribe-to-app app.act inbound-request.connection)
  ::  +request: starts handling an inbound http request
  ::
  ++  request
    |=  [secure=? =address =request:http]
    ^-  [(list move) server-state]
    =*  headers  header-list.request
    ::  for requests from localhost, respect the "forwarded" header
    ::
    =/  [secure=? =^address]
      =*  same  [secure address]
      ?.  =([%ipv4 .127.0.0.1] address)        same
      ?~  forwards=(forwarded-params headers)  same
      :-  (fall (forwarded-secure u.forwards) secure)
      (fall (forwarded-for u.forwards) address)
    ::
    =/  host  (get-header:http 'host' headers)
    =/  [=action suburl=@t]
      (get-action-for-binding host url.request)
    ::
    =/  authenticated  (request-is-logged-in:authentication request)
    ::  record that we started an asynchronous response
    ::
    =/  connection=outstanding-connection
      [action [authenticated secure address request] ~ 0]
    =.  connections.state
      (~(put by connections.state) duct connection)
    ::  figure out whether this is a cors request,
    ::  whether the origin is approved or not,
    ::  and maybe add it to the "pending approval" set
    ::
    =/  origin=(unit origin)
      (get-header:http 'origin' headers)
    =^  cors-approved  requests.cors-registry.state
      =,  cors-registry.state
      ?~  origin                         [| requests]
      ?:  (~(has in approved) u.origin)  [& requests]
      ?:  (~(has in rejected) u.origin)  [| requests]
      [| (~(put in requests) u.origin)]
    ::  if this is a cors preflight request from an approved origin
    ::  handle it synchronously
    ::
    ?:  &(?=(^ origin) cors-approved ?=(%'OPTIONS' method.request))
      %-  handle-response
      =;  =header-list:http
        [%start [204 header-list] ~ &]
      ::  allow the method and headers that were asked for,
      ::  falling back to wildcard if none specified
      ::
      ::NOTE  +handle-response will add the rest of the headers
      ::
      :~  :-  'Access-Control-Allow-Methods'
          =-  (fall - '*')
          (get-header:http 'access-control-request-method' headers)
        ::
          :-  'Access-Control-Allow-Headers'
          =-  (fall - '*')
          (get-header:http 'access-control-request-headers' headers)
      ==
    ::
    ?-    -.action
        %gen
      =/  bek=beak  [our desk.generator.action da+now]
      =/  sup=spur  path.generator.action
      =/  ski       (rof ~ %ca bek sup)
      =/  cag=cage  (need (need ski))
      ?>  =(%vase p.cag)
      =/  gat=vase  !<(vase q.cag)
      =/  res=toon
        %-  mock  :_  (look rof ~)
        :_  [%9 2 %0 1]  |.
        %+  slam
          %+  slam  gat
          !>([[now=now eny=eny bek=bek] ~ ~])
        !>([authenticated request])
      ?:  ?=(%2 -.res)
        =+  connection=(~(got by connections.state) duct)
        %^  return-static-data-on-duct  500  'text/html'
        %:  internal-server-error
            authenticated.inbound-request.connection
            url.request.inbound-request.connection
            leaf+"generator crashed"
            p.res
        ==
      ?:  ?=(%1 -.res)
        =+  connection=(~(got by connections.state) duct)
        %^  return-static-data-on-duct  500  'text/html'
        %:  internal-server-error
            authenticated.inbound-request.connection
            url.request.inbound-request.connection
            leaf+"scry blocked on"
            (fall (bind (bind ((soft path) p.res) smyt) (late ~)) ~)
        ==
      =/  result  ;;(simple-payload:http +.p.res)
      ::  ensure we have a valid content-length header
      ::
      ::    We pass on the response and the headers the generator produces, but
      ::    ensure that we have a single content-length header set correctly in
      ::    the returned if this has a body, and has no content-length if there
      ::    is no body returned to the client.
      ::
      =.  headers.response-header.result
        ?~  data.result
          (delete-header:http 'content-length' headers.response-header.result)
        ::
        %^  set-header:http  'content-length'
          (crip (format-ud-as-integer p.u.data.result))
        headers.response-header.result
      ::
      %-  handle-response
      ^-  http-event:http
      :*  %start
          response-header.result
          data.result
          complete=%.y
      ==
    ::
        %app
      :_  state
      (subscribe-to-app app.action inbound-request.connection)
    ::
        %authentication
      (handle-request:authentication secure address request)
    ::
        %logout
      (handle-logout:authentication authenticated request)
    ::
        %channel
      (handle-request:by-channel secure authenticated address request)
    ::
        %scry
      (handle-scry authenticated address request(url suburl))
    ::
        %four-oh-four
      %^  return-static-data-on-duct  404  'text/html'
      (error-page 404 authenticated url.request ~)
    ==
  ::  +handle-scry: respond with scry result, 404 or 500
  ::
  ++  handle-scry
    |=  [authenticated=? =address =request:http]
    |^  ^-  (quip move server-state)
    ?.  authenticated
      (error-response 403 ~)
    ?.  =(%'GET' method.request)
      (error-response 405 "may only GET scries")
    ::  make sure the path contains an app to scry into
    ::
    =+  req=(parse-request-line url.request)
    ?.  ?=(^ site.req)
      (error-response 400 "scry path must start with app name")
    ::  attempt the scry that was asked for
    ::
    =/  res=(unit (unit cage))
      (do-scry %gx i.site.req (snoc t.site.req (fall ext.req %mime)))
    ?~  res    (error-response 500 "failed scry")
    ?~  u.res  (error-response 404 "no scry result")
    =*  mark   p.u.u.res
    =*  vase   q.u.u.res
    ::  attempt to find conversion gate to mime
    ::
    =/  tub=(unit tube:clay)
      (find-tube i.site.req mark %mime)
    ?~  tub  (error-response 500 "no tube from {(trip mark)} to mime")
    ::  attempt conversion, then send results
    ::
    =/  mym=(each mime tang)
      (mule |.(!<(mime (u.tub vase))))
    ?-  -.mym
      %|  (error-response 500 "failed tube from {(trip mark)} to mime")
      %&  %+  return-static-data-on-duct  200
          [(rsh 3 (spat p.p.mym)) q.p.mym]
    ==
    ::
    ++  find-tube
      |=  [dap=term from=mark to=mark]
      ^-  (unit tube:clay)
      ?:  =(from to)  `(bake same vase)
      =/  des=(unit (unit cage))
        (do-scry %gd dap ~)
      ?.  ?=([~ ~ *] des)  ~
      =+  !<(=desk q.u.u.des)
      =/  tub=(unit (unit cage))
        (do-scry %cc desk /[from]/[to])
      ?.  ?=([~ ~ %tube *] tub)  ~
      `!<(tube:clay q.u.u.tub)
    ::
    ++  do-scry
      |=  [care=term =desk =path]
      ^-  (unit (unit cage))
      (rof ~ care [our desk da+now] path)
    ::
    ++  error-response
      |=  [status=@ud =tape]
      ^-  (quip move server-state)
      %^  return-static-data-on-duct  status  'text/html'
      (error-page status authenticated url.request tape)
    --
  ::  +subscribe-to-app: subscribe to app and poke it with request data
  ::
  ++  subscribe-to-app
    |=  [app=term =inbound-request:eyre]
    ^-  (list move)
    :~  :*  duct  %pass  /watch-response/[eyre-id]
            %g  %deal  [our our]  app
            %watch  /http-response/[eyre-id]
        ==
      ::
        :*  duct  %pass  /run-app-request/[eyre-id]
            %g  %deal  [our our]  app
            %poke  %handle-http-request
            !>(`[@ta inbound-request:eyre]`[eyre-id inbound-request])
        ==
    ==
  ::  +cancel-request: handles a request being externally aborted
  ::
  ++  cancel-request
    ^-  [(list move) server-state]
    ::
    ?~  connection=(~(get by connections.state) duct)
      ::  nothing has handled this connection
      ::
      [~ state]
    ::
    =.   connections.state  (~(del by connections.state) duct)
    ::
    ?-    -.action.u.connection
        %gen  [~ state]
        %app
      :_  state
      :_  ~
      :*  duct  %pass  /watch-response/[eyre-id]
          %g  %deal  [our our]  app.action.u.connection
          %leave  ~
      ==
    ::
        ?(%authentication %logout)
      [~ state]
    ::
        %channel
      on-cancel-request:by-channel
    ::
        ?(%scry %four-oh-four)
      ::  it should be impossible for a scry or 404 page to be asynchronous
      ::
      !!
    ==
  ::  +return-static-data-on-duct: returns one piece of data all at once
  ::
  ++  return-static-data-on-duct
    |=  [code=@ content-type=@t data=octs]
    ^-  [(list move) server-state]
    ::
    %-  handle-response
    :*  %start
        :-  status-code=code
        ^=  headers
          :~  ['content-type' content-type]
              ['content-length' (crip (format-ud-as-integer p.data))]
          ==
        data=[~ data]
        complete=%.y
    ==
  ::  +authentication: per-event authentication as this Urbit's owner
  ::
  ::    Right now this hard codes the authentication page using the old +code
  ::    system, but in the future should be pluggable so we can use U2F or
  ::    WebAuthn or whatever is more secure than passwords.
  ::
  ++  authentication
    |%
    ::  +handle-request: handles an http request for the login page
    ::
    ++  handle-request
      |=  [secure=? =address =request:http]
      ^-  [(list move) server-state]
      ::
      ::  if we received a simple get, just return the page
      ::
      ?:  =('GET' method.request)
        ::  parse the arguments out of request uri
        ::
        =+  request-line=(parse-request-line url.request)
        %^  return-static-data-on-duct  200  'text/html'
        (login-page (get-header:http 'redirect' args.request-line) our %.n)
      ::  if we are not a post, return an error
      ::
      ?.  =('POST' method.request)
        (return-static-data-on-duct 400 'text/html' (login-page ~ our %.n))
      ::  we are a post, and must process the body type as form data
      ::
      ?~  body.request
        (return-static-data-on-duct 400 'text/html' (login-page ~ our %.n))
      ::
      =/  parsed=(unit (list [key=@t value=@t]))
        (rush q.u.body.request yquy:de-purl:html)
      ?~  parsed
        (return-static-data-on-duct 400 'text/html' (login-page ~ our %.n))
      ::
      =/  redirect=(unit @t)  (get-header:http 'redirect' u.parsed)
      ?~  password=(get-header:http 'password' u.parsed)
        (return-static-data-on-duct 400 'text/html' (login-page redirect our %.n))
      ::  check that the password is correct
      ::
      ?.  =(u.password code)
        (return-static-data-on-duct 400 'text/html' (login-page redirect our %.y))
      ::  mint a unique session cookie
      ::
      =/  session=@uv
        |-
        =/  candidate=@uv  (~(raw og eny) 128)
        ?.  (~(has by sessions.authentication-state.state) candidate)
          candidate
        $(eny (shas %try-again candidate))
      ::  record cookie and record expiry time
      ::
      =/  first-session=?  =(~ sessions.authentication-state.state)
      =/  expires-at=@da   (add now session-timeout)
      =.  sessions.authentication-state.state
        (~(put by sessions.authentication-state.state) session [expires-at ~])
      ::
      =/  cookie-line=@t
        (session-cookie-string session &)
      ::
      =;  out=[moves=(list move) server-state]
        ::  if we didn't have any cookies previously, start the expiry timer
        ::
        ?.  first-session  out
        =-  out(moves [- moves.out])
        [duct %pass /sessions/expire %b %wait expires-at]
      ::
      ?~  redirect
        %-  handle-response
        :*  %start
            :-  status-code=204
            ^=  headers
              :~  ['set-cookie' cookie-line]
              ==
            data=~
            complete=%.y
        ==
      ::
      =/  actual-redirect  ?:(=(u.redirect '') '/' u.redirect)
      %-  handle-response
      :*  %start
          :-  status-code=303
          ^=  headers
            :~  ['location' actual-redirect]
                ['set-cookie' cookie-line]
            ==
          data=~
          complete=%.y
      ==
    ::  +handle-logout: handles an http request for logging out
    ::
    ++  handle-logout
      |=  [authenticated=? =request:http]
      ^-  [(list move) server-state]
      ::  whatever we end up doing, we always redirect to the login page
      ::
      =/  response=$>(%start http-event:http)
        :*  %start
            response-header=[303 ['location' '/~/login']~]
            data=~
            complete=%.y
        ==
      ::
      =/  session-id=(unit @uv)
        (session-id-from-request request)
      =?  headers.response-header.response  ?=(^ session-id)
        :_  headers.response-header.response
        ['set-cookie' (session-cookie-string u.session-id |)]
      ?.  &(authenticated ?=(^ session-id))
        (handle-response response)
      ::  delete the requesting session, or all sessions if so specified
      ::
      =^  channels=(list @t)  sessions.authentication-state.state
        =*  sessions  sessions.authentication-state.state
        =/  all=?
          ?~  body.request  |
          =-  ?=(^ -)
          %+  get-header:http  'all'
          (fall (rush q.u.body.request yquy:de-purl:html) ~)
        ?.  all
          :_  (~(del by sessions) u.session-id)
          %~  tap  in
          channels:(~(gut by sessions) u.session-id *session)
        :_  ~
        %~  tap  in
        %+  roll  ~(val by sessions)
        |=  [session all=(set @t)]
        (~(uni in all) channels)
      ::  close all affected channels, then send the response
      ::
      =|  moves=(list move)
      |-  ^-  (quip move server-state)
      ?~  channels
        =^  moz  state
          (handle-response response)
        [(weld moves moz) state]
      =^  moz  state
        (discard-channel:by-channel i.channels |)
      $(moves (weld moves moz), channels t.channels)
    ::  +session-id-from-request: attempt to find a session cookie
    ::
    ++  session-id-from-request
      |=  =request:http
      ^-  (unit @uv)
      ::  are there cookies passed with this request?
      ::
      ::    TODO: In HTTP2, the client is allowed to put multiple 'Cookie'
      ::    headers.
      ::
      ?~  cookie-header=(get-header:http 'cookie' header-list.request)
        ~
      ::  is the cookie line is valid?
      ::
      ?~  cookies=(rush u.cookie-header cock:de-purl:html)
        ~
      ::  is there an urbauth cookie?
      ::
      ?~  urbauth=(get-header:http (crip "urbauth-{(scow %p our)}") u.cookies)
        ~
      ::  if it's formatted like a valid session cookie, produce it
      ::
      `(unit @)`(rush u.urbauth ;~(pfix (jest '0v') viz:ag))
    ::  +request-is-logged-in: checks to see if the request is authenticated
    ::
    ::    We are considered logged in if this request has an urbauth
    ::    Cookie which is not expired.
    ::
    ++  request-is-logged-in
      |=  =request:http
      ^-  ?
      ::  does the request pass a session cookie?
      ::
      ?~  session-id=(session-id-from-request request)
        %.n
      ::  is this a session that we know about?
      ::
      ?~  session=(~(get by sessions.authentication-state.state) `@uv`u.session-id)
        %.n
      ::  is this session still valid?
      ::
      (lte now expiry-time.u.session)
    ::  +code: returns the same as |code
    ::
    ++  code
      ^-  @ta
      =/  res=(unit (unit cage))
        (rof ~ %j [our %code da+now] /(scot %p our))
      (rsh 3 (scot %p ;;(@ q.q:(need (need res)))))
    ::  +session-cookie-string: compose session cookie
    ::
    ++  session-cookie-string
      |=  [session=@uv extend=?]
      ^-  @t
      %-  crip
      =;  max-age=tape
        "urbauth-{(scow %p our)}={(scow %uv session)}; Path=/; Max-Age={max-age}"
      %-  format-ud-as-integer
      ?.  extend  0
      (div (msec:milly session-timeout) 1.000)
    --
  ::  +channel: per-event handling of requests to the channel system
  ::
  ::    Eyre offers a remote interface to your Urbit through channels, which
  ::    are persistent connections on the server which can be disconnected and
  ::    reconnected on the client.
  ::
  ++  by-channel
    ::  moves: the moves to be sent out at the end of this event, reversed
    ::
    =|  moves=(list move)
    |%
    ::  +handle-request: handles an http request for the subscription system
    ::
    ++  handle-request
      |=  [secure=? authenticated=? =address =request:http]
      ^-  [(list move) server-state]
      ::  if we're not authenticated error, but don't redirect.
      ::
      ::    We don't redirect because subscription stuff is never the toplevel
      ::    page; issuing a redirect won't help.
      ::
      ?.  authenticated
        %^  return-static-data-on-duct  403  'text/html'
        (error-page 403 authenticated url.request "unauthenticated channel usage")
      ::  parse out the path key the subscription is on
      ::
      =+  request-line=(parse-request-line url.request)
      ?.  ?=([@t @t @t ~] site.request-line)
        ::  url is not of the form '/~/channel/'
        ::
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 authenticated url.request "malformed channel url")
      ::  channel-id: unique channel id parsed out of url
      ::
      =+  channel-id=i.t.t.site.request-line
      ::
      ?:  =('PUT' method.request)
        ::  PUT methods starts/modifies a channel, and returns a result immediately
        ::
        (on-put-request channel-id request)
      ::
      ?:  =('GET' method.request)
        (on-get-request channel-id request)
      ?:  =('POST' method.request)
        ::  POST methods are used solely for deleting channels
        (on-put-request channel-id request)
      ::
      ~&  %session-not-a-put
      [~ state]
    ::  +on-cancel-request: cancels an ongoing subscription
    ::
    ::    One of our long lived sessions just got closed. We put the associated
    ::    session back into the waiting state.
    ::
    ++  on-cancel-request
      ^-  [(list move) server-state]
      ::  lookup the session id by duct
      ::
      ?~  maybe-channel-id=(~(get by duct-to-key.channel-state.state) duct)
        ~>  %slog.[0 leaf+"eyre: no channel to cancel {<duct>}"]
        [~ state]
      ::
      ~>  %slog.[0 leaf+"eyre: canceling {<duct>}"]
      ::
      =/  maybe-session
        (~(get by session.channel-state.state) u.maybe-channel-id)
      ?~  maybe-session  [~ state]
      ::
      =/  heartbeat-cancel=(list move)
        ?~  heartbeat.u.maybe-session  ~
        :~  %^  cancel-heartbeat-move
              u.maybe-channel-id
            date.u.heartbeat.u.maybe-session
          duct.u.heartbeat.u.maybe-session
        ==
      ::
      =/  expiration-time=@da  (add now channel-timeout)
      ::
      :-  %+  weld  heartbeat-cancel
        [(set-timeout-move u.maybe-channel-id expiration-time) moves]
      %_    state
          session.channel-state
        %+  ~(jab by session.channel-state.state)  u.maybe-channel-id
        |=  =channel
        ::  if we are canceling a known channel, it should have a listener
        ::
        ?>  ?=([%| *] state.channel)
        channel(state [%& [expiration-time duct]], heartbeat ~)
      ::
          duct-to-key.channel-state
        (~(del by duct-to-key.channel-state.state) duct)
      ==
    ::  +set-timeout-timer-for: sets a timeout timer on a channel
    ::
    ::    This creates a channel if it doesn't exist, cancels existing timers
    ::    if they're already set (we cannot have duplicate timers), and (if
    ::    necessary) moves channels from the listening state to the expiration
    ::    state.
    ::
    ++  update-timeout-timer-for
      |=  channel-id=@t
      ^+  ..update-timeout-timer-for
      ::  when our callback should fire
      ::
      =/  expiration-time=@da  (add now channel-timeout)
      ::  if the channel doesn't exist, create it and set a timer
      ::
      ?~  maybe-channel=(~(get by session.channel-state.state) channel-id)
        ::
        %_    ..update-timeout-timer-for
            session.channel-state.state
          %+  ~(put by session.channel-state.state)  channel-id
          [[%& expiration-time duct] 0 now ~ ~ ~ ~]
        ::
            moves
          [(set-timeout-move channel-id expiration-time) moves]
        ==
      ::  if the channel has an active listener, we aren't setting any timers
      ::
      ?:  ?=([%| *] state.u.maybe-channel)
        ..update-timeout-timer-for
      ::  we have a previous timer; cancel the old one and set the new one
      ::
      %_    ..update-timeout-timer-for
          session.channel-state.state
        %+  ~(jab by session.channel-state.state)  channel-id
        |=  =channel
        channel(state [%& [expiration-time duct]])
      ::
          moves
        :*  (cancel-timeout-move channel-id p.state.u.maybe-channel)
            (set-timeout-move channel-id expiration-time)
            moves
        ==
      ==
    ::
    ++  set-heartbeat-move
      |=  [channel-id=@t heartbeat-time=@da]
      ^-  move
      :^  duct  %pass  /channel/heartbeat/[channel-id]
      [%b %wait heartbeat-time]
    ::
    ++  cancel-heartbeat-move
      |=  [channel-id=@t heartbeat-time=@da =^duct]
      ^-  move
      :^  duct  %pass  /channel/heartbeat/[channel-id]
      [%b %rest heartbeat-time]
    ::
    ++  set-timeout-move
      |=  [channel-id=@t expiration-time=@da]
      ^-  move
      [duct %pass /channel/timeout/[channel-id] %b %wait expiration-time]
    ::
    ++  cancel-timeout-move
      |=  [channel-id=@t expiration-time=@da =^duct]
      ^-  move
      :^  duct  %pass  /channel/timeout/[channel-id]
      [%b %rest expiration-time]
    ::  +on-get-request: handles a GET request
    ::
    ::    GET requests open a channel for the server to send events to the
    ::    client in text/event-stream format.
    ::
    ++  on-get-request
      |=  [channel-id=@t =request:http]
      ^-  [(list move) server-state]
      ::  if there's no channel-id, we must 404
      ::
      ?~  maybe-channel=(~(get by session.channel-state.state) channel-id)
        %^  return-static-data-on-duct  404  'text/html'
        (error-page 404 %.y url.request ~)
      ::  when opening an event-stream, we must cancel our timeout timer
      ::  if there's no duct already bound. Else, kill the old request
      ::  and replace it
      ::
      =^  cancel-moves  state
        ?.  ?=([%| *] state.u.maybe-channel)
          :_  state
          (cancel-timeout-move channel-id p.state.u.maybe-channel)^~
        =/  cancel-heartbeat
          ?~  heartbeat.u.maybe-channel  ~
          :_  ~
          %+  cancel-heartbeat-move  channel-id
          [date duct]:u.heartbeat.u.maybe-channel
        =-  [(weld cancel-heartbeat -<) ->]
        (handle-response(duct p.state.u.maybe-channel) [%cancel ~])
      ::  the request may include a 'Last-Event-Id' header
      ::
      =/  maybe-last-event-id=(unit @ud)
        ?~  maybe-raw-header=(get-header:http 'Last-Event-ID' header-list.request)
          ~
        (rush u.maybe-raw-header dum:ag)
      ::  flush events older than the passed in 'Last-Event-ID'
      ::
      =?  state  ?=(^ maybe-last-event-id)
        (acknowledge-events channel-id u.maybe-last-event-id)
      ::  combine the remaining queued events to send to the client
      ::
      =/  event-replay=wall
        %-  zing
        %-  flop
        =/  queue  events.u.maybe-channel
        =|  events=(list wall)
        |-
        ^+  events
        ?:  =(~ queue)
          events
        =^  head  queue  ~(get to queue)
        =,  p.head
        ::NOTE  these will only fail if the mark and/or json types changed,
        ::      since conversion failure also gets caught during first receive.
        ::      we can't do anything about this, so consider it unsupported.
        =/  sign
          (channel-event-to-sign u.maybe-channel request-id channel-event)
        ?~  sign  $
        ?~  jive=(sign-to-json u.maybe-channel request-id u.sign)  $
        $(events [(event-json-to-wall id +.u.jive) events])
      ::  send the start event to the client
      ::
      =^  http-moves  state
        %-  handle-response
        :*  %start
            :-  200
            :~  ['content-type' 'text/event-stream']
                ['cache-control' 'no-cache']
                ['connection' 'keep-alive']
            ==
            (wall-to-octs event-replay)
            complete=%.n
        ==
      ::  associate this duct with this session key
      ::
      =.  duct-to-key.channel-state.state
        (~(put by duct-to-key.channel-state.state) duct channel-id)
      ::  associate this channel with the session cookie
      ::
      =.  sessions.authentication-state.state
        =/  session-id=(unit @uv)
          (session-id-from-request:authentication request)
        ?~  session-id  sessions.authentication-state.state
        %+  ~(jab by sessions.authentication-state.state)
          u.session-id
        |=  =session
        session(channels (~(put in channels.session) channel-id))
      ::  initialize sse heartbeat
      ::
      =/  heartbeat-time=@da  (add now ~s20)
      =/  heartbeat  (set-heartbeat-move channel-id heartbeat-time)
      ::  clear the event queue, record the duct for future output and
      ::  record heartbeat-time for possible future cancel
      ::
      =.  session.channel-state.state
        %+  ~(jab by session.channel-state.state)  channel-id
        |=  =channel
        channel(events ~, state [%| duct], heartbeat (some [heartbeat-time duct]))
      ::
      [[heartbeat :(weld http-moves cancel-moves moves)] state]
    ::  +acknowledge-events: removes events before :last-event-id on :channel-id
    ::
    ++  acknowledge-events
      |=  [channel-id=@t last-event-id=@u]
      ^-  server-state
      %_    state
          session.channel-state
        %+  ~(jab by session.channel-state.state)  channel-id
        |=  =channel
        ^+  channel
        =^  acked  events.channel
          (prune-events events.channel last-event-id)
        =.  unacked.channel
          (subtract-acked-events acked unacked.channel)
        channel(last-ack now)
      ==
    ::  +on-put-request: handles a PUT request
    ::
    ::    PUT requests send commands from the client to the server. We receive
    ::    a set of commands in JSON format in the body of the message.
    ::
    ++  on-put-request
      |=  [channel-id=@t =request:http]
      ^-  [(list move) server-state]
      ::  error when there's no body
      ::
      ?~  body.request
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 %.y url.request "no put body")
      ::  if the incoming body isn't json, this is a bad request, 400.
      ::
      ?~  maybe-json=(de-json:html q.u.body.request)
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 %.y url.request "put body not json")
      ::  parse the json into an array of +channel-request items
      ::
      ?~  maybe-requests=(parse-channel-request u.maybe-json)
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 %.y url.request "invalid channel json")
      ::  while weird, the request list could be empty
      ::
      ?:  =(~ u.maybe-requests)
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 %.y url.request "empty list of actions")
      ::  check for the existence of the channel-id
      ::
      ::    if we have no session, create a new one set to expire in
      ::    :channel-timeout from now. if we have one which has a timer, update
      ::    that timer.
      ::
      =.  ..on-put-request  (update-timeout-timer-for channel-id)
      ::  for each request, execute the action passed in
      ::
      =+  requests=u.maybe-requests
      ::  gall-moves: put moves here first so we can flop for ordering
      ::
      ::    TODO: Have an error state where any invalid duplicate subscriptions
      ::    or other errors cause the entire thing to fail with a 400 and a tang.
      ::
      =|  gall-moves=(list move)
      |-
      ::
      ?~  requests
        ::  this is a PUT request; we must mark it as complete
        ::
        =^  http-moves  state
          %-  handle-response
          :*  %start
              [status-code=204 headers=~]
              data=~
              complete=%.y
          ==
        ::
        [:(weld (flop gall-moves) http-moves moves) state]
      ::
      ?-    -.i.requests
          %ack
        ::  client acknowledges that they have received up to event-id
        ::
        %_  $
          state     (acknowledge-events channel-id event-id.i.requests)
          requests  t.requests
        ==
      ::
          %poke
        ::
        =.  gall-moves
          :_  gall-moves
          ^-  move
          :^  duct  %pass  /channel/poke/[channel-id]/(scot %ud request-id.i.requests)
          =,  i.requests
          :*  %g  %deal  `sock`[our ship]  app
              `task:agent:gall`[%poke-as mark %json !>(json)]
          ==
        ::
        $(requests t.requests)
      ::
          %subscribe
        ::
        =,  i.requests
        ::
        =.  gall-moves
          :_  gall-moves
          ^-  move
          :^  duct  %pass
            (subscription-wire channel-id request-id ship app)
          :*  %g  %deal  [our ship]  app
              `task:agent:gall`[%watch path]
          ==
        ::
        =.  session.channel-state.state
          %+  ~(jab by session.channel-state.state)  channel-id
          |=  =channel
          =-  channel(subscriptions -)
          %+  ~(put by subscriptions.channel)
            request-id
          [ship app path duct]
        ::
        $(requests t.requests)
      ::
          %unsubscribe
        =,  i.requests
        ::
        =/  usession  (~(get by session.channel-state.state) channel-id)
        ?~  usession
          $(requests t.requests)
        =/  subscriptions  subscriptions:u.usession
        ::
        ?~  maybe-subscription=(~(get by subscriptions) subscription-id)
          ::  the client sent us a weird request referring to a subscription
          ::  which isn't active.
          ::
          ~&  [%missing-subscription-in-unsubscribe channel-id subscription-id]
          $(requests t.requests)
        ::
        =.  gall-moves
          :_  gall-moves
          ^-  move
          =,  u.maybe-subscription
          :^  duc  %pass
            (subscription-wire channel-id subscription-id.i.requests ship app)
          :*  %g  %deal  [our ship]  app
              `task:agent:gall`[%leave ~]
          ==
        ::
        =.  session.channel-state.state
          %+  ~(jab by session.channel-state.state)  channel-id
          |=  =channel
          %_  channel
            subscriptions  (~(del by subscriptions.channel) subscription-id)
            unacked        (~(del by unacked.channel) subscription-id)
          ==
        ::
        $(requests t.requests)
      ::
          %delete
        =^  moves  state
          (discard-channel channel-id |)
        =.  gall-moves
          (weld gall-moves moves)
        $(requests t.requests)
      ::
      ==
    ::  +on-gall-response: sanity-check a gall response, send as event
    ::
    ++  on-gall-response
      |=  [channel-id=@t request-id=@ud extra=wire =sign:agent:gall]
      ^-  [(list move) server-state]
      ::  if the channel doesn't exist, we should clean up subscriptions
      ::
      ::    this is a band-aid solution. you really want eyre to have cleaned
      ::    these up on-channel-delete in the first place.
      ::    until the source of that bug is discovered though, we keep this
      ::    in place to ensure a slightly tidier home.
      ::
      ?.  ?&  !(~(has by session.channel-state.state) channel-id)
              ?=(?(%fact %watch-ack) -.sign)
              ?=([@ @ ~] extra)
          ==
        (emit-event channel-id request-id sign)
      =/  =ship     (slav %p i.extra)
      =*  app=term  i.t.extra
      =/  =tape
        %+  weld  "eyre: removing watch for "
        "non-existent channel {(trip channel-id)} on {(trip app)}"
      %-  (slog leaf+tape ~)
      :_  state
      :_  ~
      ^-  move
      :^  duct  %pass
        (subscription-wire channel-id request-id ship app)
      [%g %deal [our ship] app `task:agent:gall`[%leave ~]]
    ::  +emit-event: records an event occurred, possibly sending to client
    ::
    ::    When an event occurs, we need to record it, even if we immediately
    ::    send it to a connected browser so in case of disconnection, we can
    ::    resend it.
    ::
    ::    This function is responsible for taking the event sign and converting
    ::    it into a text/event-stream. The :sign then may get sent, and is
    ::    stored for later resending until acknowledged by the client.
    ::
    ++  emit-event
      |=  [channel-id=@t request-id=@ud =sign:agent:gall]
      ^-  [(list move) server-state]
      ::
      =/  channel=(unit channel)
        (~(get by session.channel-state.state) channel-id)
      ?~  channel
        :_  state  :_  ~
        [duct %pass /flog %d %flog %crud %eyre-no-channel >id=channel-id< ~]
      ::  it's possible that this is a sign emitted directly alongside a fact
      ::  that triggered a clog & closed the subscription. in that case, just
      ::  drop the sign.
      ::  poke-acks are not paired with subscriptions, so we can process them
      ::  regardless.
      ::
      ?:  ?&  !?=(%poke-ack -.sign)
              !(~(has by subscriptions.u.channel) request-id)
          ==
        [~ state]
      ::  attempt to convert the sign to json.
      ::  if conversion succeeds, we *can* send it. if the client is actually
      ::  connected, we *will* send it immediately.
      ::
      =/  jive=(unit (quip move json))
        (sign-to-json u.channel request-id sign)
      =/  json=(unit json)
        ?~(jive ~ `+.u.jive)
      =?  moves  ?=(^ jive)
        (weld moves -.u.jive)
      =*  sending  &(?=([%| *] state.u.channel) ?=(^ json))
      ::
      =/  next-id  next-id.u.channel
      ::  if we can send it, store the event as unacked
      ::
      =?  events.u.channel  ?=(^ json)
        %-  ~(put to events.u.channel)
        [next-id request-id (sign-to-channel-event sign)]
      ::  if it makes sense to do so, send the event to the client
      ::
      =?  moves  sending
        ^-  (list move)
        :_  moves
        ::NOTE  assertions in this block because =* is flimsy
        ?>  ?=([%| *] state.u.channel)
        :+  p.state.u.channel  %give
        ^-  gift
        :*  %response  %continue
        ::
            ^=  data
            %-  wall-to-octs
            (event-json-to-wall next-id (need json))
        ::
            complete=%.n
        ==
      =?  next-id  ?=(^ json)  +(next-id)
      ::  update channel's unacked counts, find out if clogged
      ::
      =^  clogged  unacked.u.channel
        ::  only apply clog logic to facts.
        ::  and of course don't count events we can't send as unacked.
        ::
        ?:  ?|  !?=(%fact -.sign)
                ?=(~ json)
            ==
          [| unacked.u.channel]
        =/  num=@ud
          (~(gut by unacked.u.channel) request-id 0)
        :_  (~(put by unacked.u.channel) request-id +(num))
        ?&  (gte num clog-threshold)
            (lth (add last-ack.u.channel clog-timeout) now)
        ==
      ~?  clogged  [%e %clogged channel-id request-id]
      ::  if we're clogged, or we ran into an event we can't serialize,
      ::  kill this gall subscription.
      ::
      =*  kicking    |(clogged ?=(~ json))
      =?  moves      kicking
        :_  moves
        ::NOTE  this shouldn't crash because we
        ::      - never fail to serialize subscriptionless signs (%poke-ack),
        ::      - only clog on %facts, which have a subscription associated,
        ::      - and already checked whether we still have that subscription.
        =+  (~(got by subscriptions.u.channel) request-id)
        :^  duct  %pass
          (subscription-wire channel-id request-id ship app)
        [%g %deal [our ship] app %leave ~]
      ::  update channel state to reflect the %kick
      ::
      =?  u.channel  kicking
        %_  u.channel
          subscriptions  (~(del by subscriptions.u.channel) request-id)
          unacked        (~(del by unacked.u.channel) request-id)
          events         %-  ~(put to events.u.channel)
                         [next-id request-id (sign-to-channel-event %kick ~)]
        ==
      ::  if a client is connected, send the kick event to them
      ::
      =?  moves  &(kicking ?=([%| *] state.u.channel))
        :_  moves
        :+  p.state.u.channel  %give
        ^-  gift
        :*  %response  %continue
        ::
            ^=  data
            %-  wall-to-octs
            %+  event-json-to-wall  next-id
            +:(need (sign-to-json u.channel request-id %kick ~))
        ::
            complete=%.n
        ==
      =?  next-id   kicking  +(next-id)
      ::
      :-  (flop moves)
      %_    state
          session.channel-state
        %+  ~(put by session.channel-state.state)  channel-id
        u.channel(next-id next-id)
      ==
    ::  +sign-to-channel-event: strip the vase from a sign:agent:gall
    ::
    ++  sign-to-channel-event
      |=  =sign:agent:gall
      ^-  channel-event
      ?.  ?=(%fact -.sign)  sign
      [%fact [p q.q]:cage.sign]
    ::  +app-to-desk
    ::
    ++  app-to-desk
      |=  [=channel request-id=@ud]
      ^-  (unit desk)
      =/  sub  (~(get by subscriptions.channel) request-id)
      ?~  sub
        ((slog leaf+"eyre: no subscription for request-id {<request-id>}" ~) ~)
      =/  des=(unit (unit cage))
        (rof ~ %gd [our app.u.sub da+now] ~)
      ?.  ?=([~ ~ *] des)
        ((slog leaf+"eyre: no desk for app {(trip app.u.sub)}" ~) ~)
      `!<(=desk q.u.u.des)
    ::  +channel-event-to-sign: attempt to recover a sign from a channel-event
    ::
    ++  channel-event-to-sign
      ~%  %eyre-channel-event-to-sign  ..part  ~
      |=  [=channel request-id=@ud event=channel-event]
      ^-  (unit sign:agent:gall)
      ?.  ?=(%fact -.event)  `event
      ::  rebuild vase for fact data
      ::
      =/  des=(unit desk)  (app-to-desk channel request-id)
      ?~  des  ~
      =*  have=mark  mark.event
      =/  val=(unit (unit cage))
        (rof ~ %cb [our u.des da+now] /[have])
      ?.  ?=([~ ~ *] val)
        ((slog leaf+"eyre: no mark {(trip have)}" ~) ~)
      =+  !<(=dais:clay q.u.u.val)
      =/  res  (mule |.((vale:dais noun.event)))
      ?:  ?=(%| -.res)
        ((slog leaf+"eyre: stale fact of mark {(trip have)}" ~) ~)
      `[%fact have p.res]
    ::  +sign-to-json: render sign from request-id as json channel event
    ::
    ++  sign-to-json
      ~%  %sign-to-json  ..part  ~
      |=  [=channel request-id=@ud =sign:agent:gall]
      ^-  (unit (quip move json))
      ::  for facts, we try to convert the result to json
      ::
      =/  [from=(unit [=desk =mark]) jsyn=(unit sign:agent:gall)]
        ?.  ?=(%fact -.sign)       [~ `sign]
        ?:  ?=(%json p.cage.sign)  [~ `sign]
        ::  find and use tube from fact mark to json
        ::
        =/  des=(unit desk)  (app-to-desk channel request-id)
        ?~  des  [~ ~]
        ::
        =*  have=mark  p.cage.sign
        =*  desc=tape  "from {(trip have)} to json"
        =/  convert=(unit vase)
          =/  cag=(unit (unit cage))
            (rof ~ %cf [our u.des da+now] /[have]/json)
          ?.  ?=([~ ~ *] cag)  ~
          `q.u.u.cag
        ?~  convert
          ((slog leaf+"eyre: no convert {desc}" ~) [~ ~])
        ~|  "conversion failed {desc}"
        [`[u.des have] `[%fact %json (slym u.convert q.q.cage.sign)]]
      ?~  jsyn  ~
      %-  some
      :-  ?~  from  ~
          :_  ~
          :^  duct  %pass  /conversion-cache/[mark.u.from]
          [%c %warp our desk.u.from `[%sing %f da+now /[mark.u.from]/json]]
      =*  sign  u.jsyn
      =,  enjs:format
      %-  pairs
      ^-  (list [@t json])
      :-  ['id' (numb request-id)]
      ?-    -.sign
          %poke-ack
        :~  ['response' [%s 'poke']]
          ::
            ?~  p.sign
              ['ok' [%s 'ok']]
            ['err' (wall (render-tang-to-wall 100 u.p.sign))]
        ==
      ::
          %fact
        :~  ['response' [%s 'diff']]
          ::
            :-  'json'
            ~|  [%unexpected-fact-mark p.cage.sign]
            ?>  =(%json p.cage.sign)
            !<(json q.cage.sign)
        ==
      ::
          %kick
        ['response' [%s 'quit']]~
      ::
          %watch-ack
        :~  ['response' [%s 'subscribe']]
          ::
            ?~  p.sign
              ['ok' [%s 'ok']]
            ['err' (wall (render-tang-to-wall 100 u.p.sign))]
        ==
      ==
    ::
    ++  event-json-to-wall
      ~%  %eyre-json-to-wall  ..part  ~
      |=  [event-id=@ud =json]
      ^-  wall
      :~  (weld "id: " (format-ud-as-integer event-id))
          (weld "data: " (en-json:html json))
          ""
      ==
    ::
    ++  on-channel-heartbeat
      |=  channel-id=@t
      ^-  [(list move) server-state]
      ::
      =/  res
        %-  handle-response
        :*  %continue
            data=(some (as-octs:mimes:html ':\0a'))
            complete=%.n
        ==
      =/  http-moves  -.res
      =/  new-state  +.res
      =/  heartbeat-time=@da  (add now ~s20)
      :_  %_    new-state
              session.channel-state
            %+  ~(jab by session.channel-state.state)  channel-id
            |=  =channel
            channel(heartbeat (some [heartbeat-time duct]))
          ==
      (snoc http-moves (set-heartbeat-move channel-id heartbeat-time))
    ::  +discard-channel: remove a channel from state
    ::
    ::    cleans up state, timers, and gall subscriptions of the channel
    ::
    ++  discard-channel
      |=  [channel-id=@t expired=?]
      ^-  [(list move) server-state]
      ::
      =/  usession=(unit channel)
        (~(get by session.channel-state.state) channel-id)
      ?~  usession
        [~ state]
      =/  session=channel  u.usession
      ::
      :_  %_    state
              session.channel-state
            (~(del by session.channel-state.state) channel-id)
          ::
              duct-to-key.channel-state
            ?.  ?=(%| -.state.session)  duct-to-key.channel-state.state
            (~(del by duct-to-key.channel-state.state) p.state.session)
          ==
      =/  heartbeat-cancel=(list move)
        ?~  heartbeat.session  ~
        :~  %^  cancel-heartbeat-move
              channel-id
            date.u.heartbeat.session
          duct.u.heartbeat.session
        ==
      =/  expire-cancel=(list move)
        ?:  expired  ~
        ?.  ?=(%& -.state.session)  ~
        =,  p.state.session
        [(cancel-timeout-move channel-id date duct)]~
      %+  weld  heartbeat-cancel
      %+  weld  expire-cancel
      ::  produce a list of moves which cancels every gall subscription
      ::
      %+  turn  ~(tap by subscriptions.session)
      |=  [request-id=@ud ship=@p app=term =path duc=^duct]
      ^-  move
      :^  duc  %pass
        (subscription-wire channel-id request-id ship app)
      [%g %deal [our ship] app %leave ~]
    --
  ::  +handle-gall-error: a call to +poke-http-response resulted in a %coup
  ::
  ++  handle-gall-error
    |=  =tang
    ^-  [(list move) server-state]
    ::
    =+  connection=(~(got by connections.state) duct)
    =/  moves-1=(list move)
      ?.  ?=(%app -.action.connection)
        ~
      :_  ~
      :*  duct  %pass  /watch-response/[eyre-id]
          %g  %deal  [our our]  app.action.connection
          %leave  ~
      ==
    ::
    =^  moves-2  state
      %^  return-static-data-on-duct  500  'text/html'
      ::
      %-  internal-server-error  :*
          authenticated.inbound-request.connection
          url.request.inbound-request.connection
          tang
      ==
    [(weld moves-1 moves-2) state]
  ::  +handle-response: check a response for correctness and send to earth
  ::
  ::    All outbound responses including %http-server generated responses need to go
  ::    through this interface because we want to have one centralized place
  ::    where we perform logging and state cleanup for connections that we're
  ::    done with.
  ::
  ++  handle-response
    |=  =http-event:http
    ^-  [(list move) server-state]
    ::  verify that this is a valid response on the duct
    ::
    ?~  connection-state=(~(get by connections.state) duct)
      ~&  [%invalid-outstanding-connection duct]
      [~ state]
    ::
    |^  ^-  [(list move) server-state]
        ::
        ?-    -.http-event
        ::
            %start
          ?^  response-header.u.connection-state
            ~&  [%http-multiple-start duct]
            error-connection
          ::  if request was authenticated, extend the session & cookie's life
          ::
          =^  response-header  sessions.authentication-state.state
            =,  authentication
            =*  sessions  sessions.authentication-state.state
            =*  inbound   inbound-request.u.connection-state
            =*  no-op     [response-header.http-event sessions]
            ::
            ?.  authenticated.inbound
              no-op
            ?~  session-id=(session-id-from-request request.inbound)
              ::  cookies are the only auth method, so this is unexpected
              ::
              ~&  [%e %authenticated-without-cookie]
              no-op
            ?.  (~(has by sessions) u.session-id)
              ::  if the session has expired since the request was opened,
              ::  tough luck, we don't create/revive sessions here
              ::
              no-op
            :_  %+  ~(jab by sessions)  u.session-id
                |=  =session
                session(expiry-time (add now session-timeout))
            =-  response-header.http-event(headers -)
            %^  set-header:http  'set-cookie'
              (session-cookie-string u.session-id &)
            headers.response-header.http-event
          ::
          =/  connection=outstanding-connection
            (~(got by connections.state) duct)
          ::  if the request was a simple cors request from an approved origin
          ::  append the necessary cors headers to the response
          ::
          =/  origin=(unit origin)
            %+  get-header:http  'origin'
            header-list.request.inbound-request.connection
          =?  headers.response-header
              ?&  ?=(^ origin)
                  (~(has in approved.cors-registry.state) u.origin)
              ==
            %^  set-header:http  'Access-Control-Allow-Origin'       u.origin
            %^  set-header:http  'Access-Control-Allow-Credentials'  'true'
            headers.response-header
          ::
          =.  response-header.http-event  response-header
          =.  connections.state
            %+  ~(put by connections.state)  duct
            %_  connection
              response-header  `response-header
              bytes-sent  ?~(data.http-event 0 p.u.data.http-event)
            ==
          ::
          =?  state  complete.http-event
            log-complete-request
          ::
          pass-response
        ::
            %continue
          ?~  response-header.u.connection-state
            ~&  [%http-continue-without-start duct]
            error-connection
          ::
          =.  connections.state
            %+  ~(jab by connections.state)  duct
            |=  connection=outstanding-connection
            =+  size=?~(data.http-event 0 p.u.data.http-event)
            connection(bytes-sent (add bytes-sent.connection size))
          ::
          =?  state  complete.http-event
            log-complete-request
          ::
          pass-response
        ::
            %cancel
          ::  todo: log this differently from an ise.
          ::
          error-connection
        ==
    ::
    ++  pass-response
      ^-  [(list move) server-state]
      [[duct %give %response http-event]~ state]
    ::
    ++  log-complete-request
      ::  todo: log the complete request
      ::
      ::  remove all outstanding state for this connection
      ::
      =.  connections.state
        (~(del by connections.state) duct)
      state
    ::
    ++  error-connection
      ::  todo: log application error
      ::
      ::  remove all outstanding state for this connection
      ::
      =.  connections.state
        (~(del by connections.state) duct)
      ::  respond to outside with %error
      ::
      ^-  [(list move) server-state]
      :_  state
      :-  [duct %give %response %cancel ~]
      ?.  ?=(%app -.action.u.connection-state)
        ~
      :_  ~
      :*  duct  %pass  /watch-response/[eyre-id]
          %g  %deal  [our our]  app.action.u.connection-state
          %leave  ~
      ==
    --
  ::  +add-binding: conditionally add a pairing between binding and action
  ::
  ::    Adds =binding =action if there is no conflicting bindings.
  ::
  ++  add-binding
    |=  [=binding =action]
    ^-  [(list move) server-state]
    =^  success  bindings.state
      ::  prevent binding in reserved namespaces
      ::
      ?:  ?|  ?=([%'~' *] path.binding)    ::  eyre
              ?=([%'~_~' *] path.binding)  ::  runtime
          ==
        [| bindings.state]
      [& (insert-binding [binding duct action] bindings.state)]
    :_  state
    [duct %give %bound & binding]~
  ::  +remove-binding: removes a binding if it exists and is owned by this duct
  ::
  ++  remove-binding
    |=  =binding
    ::
    ^-  server-state
    %_    state
        bindings
      %+  skip  bindings.state
      |=  [item-binding=^binding item-duct=^duct =action]
      ^-  ?
      &(=(item-binding binding) =(item-duct duct))
    ==
  ::  +get-action-for-binding: finds an action for an incoming web request
  ::
  ++  get-action-for-binding
    |=  [raw-host=(unit @t) url=@t]
    ^-  [=action suburl=@t]
    ::  process :raw-host
    ::
    ::    If we are missing a 'Host:' header, if that header is a raw IP
    ::    address, or if the 'Host:' header refers to [our].urbit.org, we want
    ::    to return ~ which means we're unidentified and will match against any
    ::    wildcard matching.
    ::
    ::    Otherwise, return the site given.
    ::
    =/  host=(unit @t)
      ?~  raw-host
        ~
      ::  Parse the raw-host so that we can ignore ports, usernames, etc.
      ::
      =+  parsed=(rush u.raw-host simplified-url-parser)
      ?~  parsed
        ~
      ::  if the url is a raw IP, assume default site.
      ::
      ?:  ?=([%ip *] -.u.parsed)
        ~
      ::  if the url is "localhost", assume default site.
      ::
      ?:  =([%site 'localhost'] -.u.parsed)
        ~
      ::  render our as a tape, and cut off the sig in front.
      ::
      =/  with-sig=tape  (scow %p our)
      ?>  ?=(^ with-sig)
      ?:  =(u.raw-host (crip t.with-sig))
        ::  [our].urbit.org is the default site
        ::
        ~
      ::
      raw-host
    ::  url is the raw thing passed over the 'Request-Line'.
    ::
    ::    todo: this is really input validation, and we should return a 500 to
    ::    the client.
    ::
    =/  request-line  (parse-request-line url)
    =/  parsed-url=(list @t)  site.request-line
    ::
    =/  bindings  bindings.state
    |-
    ::
    ?~  bindings
      [[%four-oh-four ~] url]
    ::
    ?.  (host-matches site.binding.i.bindings raw-host)
      $(bindings t.bindings)
    ?~  suffix=(find-suffix path.binding.i.bindings parsed-url)
      $(bindings t.bindings)
    ::
    :-  action.i.bindings
    %^  cat  3
      %+  roll
        ^-  (list @t)
        (join '/' (flop ['' u.suffix]))
      (cury cat 3)
    ?~  ext.request-line  ''
    (cat 3 '.' u.ext.request-line)
  --
::
++  forwarded-params
  |=  =header-list:http
  ^-  (unit (list (map @t @t)))
  %+  biff
    (get-header:http 'forwarded' header-list)
  unpack-header:http
::
++  forwarded-for
  |=  forwards=(list (map @t @t))
  ^-  (unit address)
  ?.  ?=(^ forwards)  ~
  =*  forward  i.forwards
  ?~  for=(~(get by forward) 'for')  ~
  ::NOTE  per rfc7239, non-ip values are also valid. they're not useful
  ::      for the general case, so we ignore them here. if needed,
  ::      request handlers are free to inspect the headers themselves.
  ::
  %+  rush  u.for
  ;~  sfix
    ;~(pose (stag %ipv4 ip4) (stag %ipv6 (ifix [sel ser] ip6)))
    ;~(pose ;~(pfix col dim:ag) (easy ~))
  ==
::
++  forwarded-secure
  |=  forwards=(list (map @t @t))
  ^-  (unit ?)
  ?.  ?=(^ forwards)  ~
  =*  forward  i.forwards
  ?~  proto=(~(get by forward) 'proto')  ~
  ?+  u.proto  ~
    %http   `|
    %https  `&
  ==
::
++  parse-request-line
  |=  url=@t
  ^-  [[ext=(unit @ta) site=(list @t)] args=(list [key=@t value=@t])]
  (fall (rush url ;~(plug apat:de-purl:html yque:de-purl:html)) [[~ ~] ~])
::  +insert-binding: add a new binding, replacing any existing at its path
::
++  insert-binding
  |=  $:  new=[=binding =duct =action]
          bindings=(list [=binding =duct =action])
      ==
  ^+  bindings
  ?~  bindings  [new]~
  =*  bid  binding.i.bindings
  ::  replace already bound paths
  ::
  ?:  =([site path]:bid [site path]:binding.new)
    ~>  %slog.[0 leaf+"eyre: replacing existing binding at {<`path`path.bid>}"]
    [new t.bindings]
  ::  if new comes before bid, prepend it.
  ::  otherwise, continue our search.
  ::
  =;  new-before-bid=?
    ?:  new-before-bid  [new bindings]
    [i.bindings $(bindings t.bindings)]
  ?:  =(site.binding.new site.bid)
    (aor path.bid path.binding.new)
  (aor (fall site.bid '') (fall site.binding.new ''))
::
++  channel-wire
  |=  [channel-id=@t request-id=@ud]
  ^-  wire
  /channel/subscription/[channel-id]/(scot %ud request-id)
::
++  subscription-wire
  |=  [channel-id=@t request-id=@ud =ship app=term]
  ^-  wire
  (weld (channel-wire channel-id request-id) /(scot %p ship)/[app])
--
::  end the =~
::
.  ==
::  begin with a default +axle as a blank slate
::
=|  ax=axle
::  a vane is activated with current date, entropy, and a namespace function
::
|=  [now=@da eny=@uvJ rof=roof]
::  allow jets to be registered within this core
::
~%  %http-server  ..part  ~
|%
++  call
  ~/  %eyre-call
  |=  [=duct dud=(unit goof) wrapped-task=(hobo task)]
  ^-  [(list move) _http-server-gate]
  ::
  =/  task=task  ((harden task) wrapped-task)
  ::
  ::  XX handle error notifications
  ::
  ?^  dud
    =/  moves=(list move)
      [[duct %slip %d %flog %crud [-.task tang.u.dud]] ~]
    [moves http-server-gate]
  ::  %init: tells us what our ship name is
  ::
  ?:  ?=(%init -.task)
    ::  initial value for the login handler
    ::
    =.  bindings.server-state.ax
      =-  (roll - insert-binding)
      ^-  (list [binding ^duct action])
      :~  [[~ /~/login] duct [%authentication ~]]
          [[~ /~/logout] duct [%logout ~]]
          [[~ /~/channel] duct [%channel ~]]
          [[~ /~/scry] duct [%scry ~]]
      ==
    [~ http-server-gate]
  ::  %trim: in response to memory pressure
  ::
  ::    Cancel all inactive channels
  ::    XX cancel active too if =(0 trim-priority) ?
  ::
  ?:  ?=(%trim -.task)
    =/  event-args  [[eny duct now rof] server-state.ax]
    =*  by-channel  by-channel:(per-server-event event-args)
    =*  channel-state  channel-state.server-state.ax
    ::
    =/  inactive=(list @t)
      =/  full=(set @t)  ~(key by session.channel-state)
      =/  live=(set @t)
        (~(gas in *(set @t)) ~(val by duct-to-key.channel-state))
      ~(tap in (~(dif in full) live))
    ::
    ?:  =(~ inactive)
      [~ http-server-gate]
    ::
    =/  len=tape  (scow %ud (lent inactive))
    ~>  %slog.[0 leaf+"eyre: trim: closing {len} inactive channels"]
    ::
    =|  moves=(list (list move))
    |-  ^-  [(list move) _http-server-gate]
    =*  channel-id  i.inactive
    ?~  inactive
      [(zing (flop moves)) http-server-gate]
    ::  discard channel state, and cancel any active gall subscriptions
    ::
    =^  mov  server-state.ax  (discard-channel:by-channel channel-id |)
    $(moves [mov moves], inactive t.inactive)
  ::
  ::  %vega: notifies us of a completed kernel upgrade
  ::
  ?:  ?=(%vega -.task)
    [~ http-server-gate]
  ::  %born: new unix process
  ::
  ?:  ?=(%born -.task)
    ::  close previously open connections
    ::
    ::    When we have a new unix process, every outstanding open connection is
    ::    dead. For every duct, send an implicit close connection.
    ::
    =^  closed-connections=(list move)  server-state.ax
      =/  connections=(list [=^duct *])
        ~(tap by connections.server-state.ax)
      ::
      =|  closed-connections=(list move)
      |-
      ?~  connections
        [closed-connections server-state.ax]
      ::
      =/  event-args
        [[eny duct.i.connections now rof] server-state.ax]
      =/  cancel-request  cancel-request:(per-server-event event-args)
      =^  moves  server-state.ax  cancel-request
      ::
      $(closed-connections (weld moves closed-connections), connections t.connections)
    ::  save duct for future %give to unix
    ::
    =.  outgoing-duct.server-state.ax  duct
    ::
    :_  http-server-gate
    ;:  weld
      ::  hand back default configuration for now
      ::
      [duct %give %set-config http-config.server-state.ax]~
    ::
      closed-connections
    ==
  ::
  ?:  ?=(%code-changed -.task)
    ~>  %slog.[0 leaf+"eyre: code-changed: throwing away cookies and sessions"]
    =.  authentication-state.server-state.ax  *authentication-state
    ::
    =/  event-args  [[eny duct now rof] server-state.ax]
    =*  by-channel  by-channel:(per-server-event event-args)
    =*  channel-state  channel-state.server-state.ax
    ::
    =/  channel-ids=(list @t)  ~(tap in ~(key by session.channel-state))
    =|  moves=(list (list move))
    |-  ^-  [(list move) _http-server-gate]
    ?~  channel-ids
      [(zing (flop moves)) http-server-gate]
    ::  discard channel state, and cancel any active gall subscriptions
    ::
    =^  mov  server-state.ax  (discard-channel:by-channel i.channel-ids |)
    $(moves [mov moves], channel-ids t.channel-ids)
  ::
  ::  all other commands operate on a per-server-event
  ::
  =/  event-args  [[eny duct now rof] server-state.ax]
  =/  server  (per-server-event event-args)
  ::
  ?-    -.task
      ::  %live: notifies us of the ports of our live http servers
      ::
      %live
    =.  ports.server-state.ax  +.task
    [~ http-server-gate]
      ::  %rule: updates our http configuration
      ::
      %rule
    ?-  -.http-rule.task
        ::  %cert: install tls certificate
        ::
        %cert
      =*  config  http-config.server-state.ax
      ?:  =(secure.config cert.http-rule.task)
        [~ http-server-gate]
      =.  secure.config  cert.http-rule.task
      :_  http-server-gate
      =*  out-duct  outgoing-duct.server-state.ax
      ?~  out-duct  ~
      [out-duct %give %set-config config]~
        ::  %turf: add or remove domain name
        ::
        %turf
      =*  domains  domains.server-state.ax
      =/  mod=(set turf)
        ?:  ?=(%put action.http-rule.task)
          (~(put in domains) turf.http-rule.task)
        (~(del in domains) turf.http-rule.task)
      ?:  =(domains mod)
        [~ http-server-gate]
      =.  domains  mod
      :_  http-server-gate
      =/  cmd
        [%acme %poke `cage`[%acme-order !>(mod)]]
      [duct %pass /acme/order %g %deal [our our] cmd]~
    ==
  ::
      %request
    =^  moves  server-state.ax  (request:server +.task)
    [moves http-server-gate]
  ::
      %request-local
    =^  moves  server-state.ax  (request-local:server +.task)
    [moves http-server-gate]
  ::
      %cancel-request
    =^  moves  server-state.ax  cancel-request:server
    [moves http-server-gate]
  ::
      %connect
    =^  moves  server-state.ax
      %+  add-binding:server  binding.task
      [%app app.task]
    [moves http-server-gate]
  ::
      %serve
    =^  moves  server-state.ax
      %+  add-binding:server  binding.task
      [%gen generator.task]
    [moves http-server-gate]
  ::
      %disconnect
    =.  server-state.ax  (remove-binding:server binding.task)
    [~ http-server-gate]
  ::
      %approve-origin
    =.  cors-registry.server-state.ax
      =,  cors-registry.server-state.ax
      :+  (~(del in requests) origin.task)
        (~(put in approved) origin.task)
      (~(del in rejected) origin.task)
    [~ http-server-gate]
  ::
      %reject-origin
    =.  cors-registry.server-state.ax
      =,  cors-registry.server-state.ax
      :+  (~(del in requests) origin.task)
        (~(del in approved) origin.task)
      (~(put in rejected) origin.task)
    [~ http-server-gate]
  ==
::
++  take
  ~/  %eyre-take
  |=  [=wire =duct dud=(unit goof) =sign]
  ^-  [(list move) _http-server-gate]
  ?^  dud
    ~|(%eyre-take-dud (mean tang.u.dud))
  =>  %=    .
          sign
        ?:  ?=(%gall -.sign)
          ?>  ?=(%unto +<.sign)
          sign
        sign
      ==
  ::  :wire must at least contain two parts, the type and the build
  ::
  ?>  ?=([@ *] wire)
  ::
  |^  ^-  [(list move) _http-server-gate]
      ::
      ?+    i.wire
          ~|([%bad-take-wire wire] !!)
      ::
        %run-app-request   run-app-request
        %watch-response    watch-response
        %sessions          sessions
        %channel           channel
        %acme              acme-ack
        %conversion-cache  `http-server-gate
      ==
  ::
  ++  run-app-request
    ::
    ?>  ?=([%gall %unto *] sign)
    ::
    ::
    ?>  ?=([%poke-ack *] p.sign)
    ?>  ?=([@ *] t.wire)
    ?~  p.p.sign
      ::  received a positive acknowledgment: take no action
      ::
      [~ http-server-gate]
    ::  we have an error; propagate it to the client
    ::
    =/  event-args  [[eny duct now rof] server-state.ax]
    =/  handle-gall-error
      handle-gall-error:(per-server-event event-args)
    =^  moves  server-state.ax
      (handle-gall-error u.p.p.sign)
    [moves http-server-gate]
  ::
  ++  watch-response
    ::
    =/  event-args  [[eny duct now rof] server-state.ax]
    ::
    ?>  ?=([@ *] t.wire)
    ?:  ?=([%gall %unto %watch-ack *] sign)
      ?~  p.p.sign
        ::  received a positive acknowledgment: take no action
        ::
        [~ http-server-gate]
      ::  we have an error; propagate it to the client
      ::
      =/  handle-gall-error
        handle-gall-error:(per-server-event event-args)
      =^  moves  server-state.ax  (handle-gall-error u.p.p.sign)
      [moves http-server-gate]
    ::
    ?:  ?=([%gall %unto %kick ~] sign)
      =/  handle-response  handle-response:(per-server-event event-args)
      =^  moves  server-state.ax
        (handle-response %continue ~ &)
      [moves http-server-gate]
    ::
    ?>  ?=([%gall %unto %fact *] sign)
    =/  =mark  p.cage.p.sign
    =/  =vase  q.cage.p.sign
    ?.  ?=  ?(%http-response-header %http-response-data %http-response-cancel)
        mark
      =/  handle-gall-error
        handle-gall-error:(per-server-event event-args)
      =^  moves  server-state.ax
        (handle-gall-error leaf+"eyre bad mark {(trip mark)}" ~)
      [moves http-server-gate]
    ::
    =/  =http-event:http
      ?-  mark
        %http-response-header  [%start !<(response-header:http vase) ~ |]
        %http-response-data    [%continue !<((unit octs) vase) |]
        %http-response-cancel  [%cancel ~]
      ==
    =/  handle-response  handle-response:(per-server-event event-args)
    =^  moves  server-state.ax
      (handle-response http-event)
    [moves http-server-gate]
  ::
  ++  channel
    ::
    =/  event-args  [[eny duct now rof] server-state.ax]
    ::  channel callback wires are triples.
    ::
    ?>  ?=([@ @ @t *] wire)
    ::
    ?+    i.t.wire
        ~|([%bad-channel-wire wire] !!)
    ::
        %timeout
      ?>  ?=([%behn %wake *] sign)
      ?^  error.sign
        [[duct %slip %d %flog %crud %wake u.error.sign]~ http-server-gate]
      =/  discard-channel
        discard-channel:by-channel:(per-server-event event-args)
      =^  moves  server-state.ax
        (discard-channel i.t.t.wire &)
      [moves http-server-gate]
    ::
        %heartbeat
      =/  on-channel-heartbeat
        on-channel-heartbeat:by-channel:(per-server-event event-args)
      =^  moves  server-state.ax
        (on-channel-heartbeat i.t.t.wire)
      [moves http-server-gate]
    ::
        ?(%poke %subscription)
      ?>  ?=([%gall %unto *] sign)
      ~|  eyre-sub=wire
      ?>  ?=([@ @ @t @ *] wire)
      ?<  ?=(%raw-fact -.p.sign)
      =*  channel-id  i.t.t.wire
      =*  request-id  i.t.t.t.wire
      =*  extra-wire  t.t.t.t.wire
      =/  on-gall-response
        on-gall-response:by-channel:(per-server-event event-args)
      ::  ~&  [%gall-response sign]
      =^  moves  server-state.ax
        %-  on-gall-response
        [channel-id (slav %ud request-id) extra-wire p.sign]
      [moves http-server-gate]
    ==
  ::
  ++  sessions
    ::
    ?>  ?=([%behn %wake *] sign)
    ::
    ?^  error.sign
      [[duct %slip %d %flog %crud %wake u.error.sign]~ http-server-gate]
    ::  remove cookies that have expired
    ::
    =*  sessions  sessions.authentication-state.server-state.ax
    =.  sessions.authentication-state.server-state.ax
      %-  ~(gas by *(map @uv session))
      %+  skip  ~(tap in sessions)
      |=  [cookie=@uv session]
      (lth expiry-time now)
    ::  if there's any cookies left, set a timer for the next expected expiry
    ::
    ^-  [(list move) _http-server-gate]
    :_  http-server-gate
    ?:  =(~ sessions)  ~
    =;  next-expiry=@da
      [duct %pass /sessions/expire %b %wait next-expiry]~
    %+  roll  ~(tap by sessions)
    |=  [[@uv session] next=@da]
    ?:  =(*@da next)  expiry-time
    (min next expiry-time)
  ::
  ++  acme-ack
    ?>  ?=([%gall %unto *] sign)
    ::
    ?>  ?=([%poke-ack *] p.sign)
    ?~  p.p.sign
      ::  received a positive acknowledgment: take no action
      ::
      [~ http-server-gate]
    ::  received a negative acknowledgment: XX do something
    ::
    [((slog u.p.p.sign) ~) http-server-gate]
  --
::
++  http-server-gate  ..$
::  +load: migrate old state to new state (called on vane reload)
::
++  load
  |=  old=axle
  ^+  ..^$
  ..^$(ax old)
::  +stay: produce current state
::
++  stay  `axle`ax
::  +scry: request a path in the urbit namespace
::
++  scry
  ~/  %eyre-scry
  ^-  roon
  |=  [lyc=gang car=term bem=beam]
  ^-  (unit (unit cage))
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =/  lot=coin  $/r.bem
  =*  tyl  s.bem
  ::
  ?.  ?=(%& -.why)
    ~
  =*  who  p.why
  ?:  =(tyl /whey)
    =/  maz=(list mass)
      :~  bindings+&+bindings.server-state.ax
          auth+&+authentication-state.server-state.ax
          connections+&+connections.server-state.ax
          channels+&+channel-state.server-state.ax
          axle+&+ax
      ==
    ``mass+!>(maz)
  ?.  ?=(%$ -.lot)
    [~ ~]
  ?.  =(our who)
    ?.  =([%da now] p.lot)
      [~ ~]
    ~&  [%r %scry-foreign-host who]
    ~
  ?:  &(?=(%x ren) ?=(~ syd))
    =,  server-state.ax
    ?+  tyl  [~ ~]
      [%cors ~]            ``noun+!>(cors-registry)
      [%cors %requests ~]  ``noun+!>(requests.cors-registry)
      [%cors %approved ~]  ``noun+!>(approved.cors-registry)
      [%cors %rejected ~]  ``noun+!>(rejected.cors-registry)
    ::
        [%cors ?(%approved %rejected) @ ~]
      =*  kind  i.t.tyl
      =*  orig  i.t.t.tyl
      ?~  origin=(slaw %t orig)  [~ ~]
      ?-  kind
        %approved  ``noun+!>((~(has in approved.cors-registry) u.origin))
        %rejected  ``noun+!>((~(has in rejected.cors-registry) u.origin))
      ==
    ::
        [%authenticated %cookie @ ~]
      ?~  cookies=(slaw %t i.t.t.tyl)  [~ ~]
      :^  ~  ~  %noun
      !>  ^-  ?
      %-  =<  request-is-logged-in:authentication
          (per-server-event [eny *duct now rof] server-state.ax)
      %*(. *request:http header-list ['cookie' u.cookies]~)
    ==
  ?.  ?=(%$ ren)
    [~ ~]
  ?+  syd  [~ ~]
    %bindings              ``noun+!>(bindings.server-state.ax)
    %connections           ``noun+!>(connections.server-state.ax)
    %authentication-state  ``noun+!>(authentication-state.server-state.ax)
    %channel-state         ``noun+!>(channel-state.server-state.ax)
  ::
      %host
    %-  (lift (lift |=(a=hart:eyre [%hart !>(a)])))
    ^-  (unit (unit hart:eyre))
    =.  p.lot  ?.(=([%da now] p.lot) p.lot [%tas %real])
    ?+  p.lot
      [~ ~]
    ::
        [%tas %fake]
      ``[& [~ 8.443] %& /localhost]
    ::
        [%tas %real]
      =*  domains  domains.server-state.ax
      =*  ports  ports.server-state.ax
      =/  =host:eyre  [%& ?^(domains n.domains /localhost)]
      =/  secure=?  &(?=(^ secure.ports) !?=(hoke:eyre host))
      =/  port=(unit @ud)
        ?.  secure
          ?:(=(80 insecure.ports) ~ `insecure.ports)
        ?>  ?=(^ secure.ports)
        ?:(=(443 u.secure.ports) ~ secure.ports)
      ``[secure port host]
    ==
  ==
--
