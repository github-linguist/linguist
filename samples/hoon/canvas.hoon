::  canvas: A p2p drawing app
::
::    data:            scry command:
::    ------------    ----------------------------------------------
::    canvas           .^(canvas %gx /=canvas=/canvas/<ship>/<canvas>/noun)
::    gallery          .^((list canvas) %gx /=canvas=/gallery/noun)
::
::  State
::
/-  *canvas, file-server, launch-store
/+  *server, default-agent, verb, dbug,
    *canvas,
    *canvas-templates
::
=>  |%
    +$  card  card:agent:gall
    ::
    +$  state-zero
      $:  %0
          gallery=(map location canvas)
      ==
    --
::
=|  state-zero
=*  state  -
::  Main
::
%-  agent:dbug
^-  agent:gall
=<  |_  =bowl:gall
    +*  this         .
        canvas-core  +>
        cc           ~(. canvas-core bowl)
        def          ~(. (default-agent this %|) bowl)
    ::
    ++  on-init
      ^-  (quip card _this)
      ~&  >  "[ init canvas - / welcome / ]"
      =|  cards=(list card)
      =/  name=@t  'welcome'
      =/  =canvas  (welcome ~ name our.bowl &)
      =.  gallery.state  (~(put by gallery) [[our.bowl name] canvas])
      =/  has-file=?
        (gall-scry:cc ? %file-server /url/'~canvas'/noun)
      =/  has-tile=?
        (~(has in (gall-scry:cc (set @tas) %launch /keys/noun)) %canvas)
      =?  cards  !has-file
        =/  file=action:file-server
          [%serve-dir /'~canvas' /app/canvas %.n %.y]
        :_  cards
        (poke-our:cc %file-server %file-server-action !>(file))
      =?  cards  !has-tile
        =/  tile=action:launch-store
          [%add %canvas [%custom `'/~canvas' `'/~canvas/img/tile.svg'] %.y]
        :_  cards
        (poke-our:cc %launch %launch-action !>(tile))
      ::
      [cards this]
    ::
    ++  on-save  !>(state)
    ::
    ++  on-load
      |=  old=vase
      ^-  (quip card _this)
      =|  cards=(list card)
      =/  has-file=?
        (gall-scry:cc ? %file-server /url/'~canvas'/noun)
      =/  has-tile=?
        (~(has in (gall-scry:cc (set @tas) %launch /keys/noun)) %canvas)
      =?  cards  !has-file
        =/  file=action:file-server
          [%serve-dir /'~canvas' /app/canvas %.n %.y]
        :_  cards
        (poke-our:cc %file-server %file-server-action !>(file))
      =?  cards  !has-tile
        =/  tile=action:launch-store
          [%add %canvas [%custom `'/~canvas' `'/~canvas/img/tile.svg'] %.y]
        :_  cards
        (poke-our:cc %launch %launch-action !>(tile))
      ::
      [cards this(state !<(state-zero old))]
    ::
    ++  on-poke
      |=  [=mark =vase]
      ^-  (quip card _this)
      ?+    mark  (on-poke:def mark vase)
          %canvas-action
        =^  cards  state
          (handle-canvas-action:cc !<(canvas-action vase))
        [cards this]
      ::
          %json
        ?>  (team:title our.bowl src.bowl)
        =^  cards  state
          %-  handle-canvas-action:cc
          (json-to-canvas-view !<(json vase))
        [cards this]
      ==
    ::
    ++  on-watch
      |=  =path
      ^-  (quip card _this)
      :_  this
      ?+    path  ~|([%peer-canvas-strange path] !!)
          [%frontend *]
        =^  cards  state
          (handle-canvas-action:cc [%init ~])
        cards
      ::
          [%canvas ^]
        =^  cards  state
           (send-init-canvas:cc i.t.path)
        cards
      ==
    ::
    ++  on-agent
      |=  [=wire =sign:agent:gall]
      ^-  (quip card _this)
      ?-    -.sign
          %poke-ack   (on-agent:def wire sign)
          %watch-ack  (on-agent:def wire sign)
          %kick       (on-agent:def wire sign)
      ::
          %fact
        =^  cards  state
          =*  vase  q.cage.sign
          ^-  (quip card _state)
          ?+    p.cage.sign  ~|([%canvas-bad-update-mark wire vase] !!)
              %canvas-update
            (handle-canvas-update:cc !<(canvas-update vase))
          ==
        [cards this]
      ==
    ::
    ++  on-arvo
      |=  [=wire =sign-arvo]
      ^-  (quip card:agent:gall _this)
      ?:  ?=(%bound +<.sign-arvo)  [~ this]
      (on-arvo:def wire sign-arvo)
    ::
    ++  on-leave  on-leave:def
    ::
    ++  on-peek
      |=  =path
      ^-  (unit (unit cage))
      ?.  (team:title our.bowl src.bowl)  ~
      ?+    path  (on-peek:def path)
          [%x %canvas @t @t ~]

        =+  out=(~(got by gallery) (extract-location t.t.path))
        ``noun+!>(out)
      ::
          [%x %gallery ~]
        ``noun+!>(~(val by gallery))
      ==
    ::
    ++  on-fail   on-fail:def
    --
::
|_  =bowl:gall
::
++  handle-canvas-action
  |=  act=canvas-action
  ^-  (quip card _state)
  |^
  ?+  -.act  !!
    %init    [handle-init state]
    %paint   (handle-paint +.act)
    %join    (handle-join +.act)
    %leave   (handle-leave +.act)
    %create  (handle-create +.act)
    %save    (handle-save +.act)
    %unlock  (handle-unlock +.act)
  ==
  ::
  ++  handle-init
    ^-  (list card)
    %-  send-frontend
    (canvas-view-response-to-json %init-frontend ~(val by gallery) ~)
  ::
  ++  handle-paint
    |=  [location=@p name=@t strokes=(list stroke)]
    ^-  (quip card _state)
    (process-remote-paint location name strokes)
  ::
  ++  handle-join
    |=  [=ship name=@t]
    ^-  (quip card _state)
    ?>  (team:title our.bowl src.bowl)
    ~&  >  "[ join ]"
    [[(subscribe ship name)]~ state]
  ::
  ++  handle-leave
    |=  [=ship name=@t]
    ^-  (quip card _state)
    ~&  >>  ["[ leave ]" ship name]
    ?>  (team:title our.bowl src.bowl)
    =/  new-name=@t
      (crip "{(trip name)}-{(trip (scot %da now.bowl))}")
    =/  =canvas  (~(got by gallery) [ship name])
    ::  the canvas becomes local and private
    ::  and it's archived under the old name and the date
    ::
    =.  metadata.canvas
      %_  metadata.canvas
        private   &
        location  our.bowl
        name      new-name
      ==
    =/  load
      ::[%give %fact [/updates]~ %canvas-action !>([%load new-name canvas])]
      (send-frontend (canvas-view-response-to-json %load new-name canvas))
    :-  [(leave ship name) load]
    %_    state
        gallery
      (~(put by (~(del by gallery) [ship name])) [our.bowl new-name] canvas)
    ==
  ::
  ++  handle-create
    |=  =canvas
    ^-  (quip card _state)
    ?>  (team:title our.bowl src.bowl)
    =*  name  name.metadata.canvas
    =*  private  private.metadata.canvas
    ~&  >  ["[ create ]" name]
    =.  canvas
      %.  [canvas name our.bowl private]
      ?+  template.metadata.canvas  blank
        %mesh-welcome    welcome
        %mesh-monkey     monkey
        %mesh-homer      homer
        %mesh-dumas      dumas-dutil
        %mesh-hackathon  hackathon
      ==
    =/  load=canvas-view-response
      [%load name.metadata.canvas canvas]
    :-  (send-frontend (canvas-view-response-to-json load))
    %_    state
        gallery
      (~(put by gallery) [[our.bowl name.metadata.canvas] canvas])
    ==
  ::
  ++  handle-save
    |=  [=ship name=@t file=@t]
    ^-  (quip card _state)
    ?>  (team:title our.bowl src.bowl)
    =/  canvas=(unit canvas)  (~(get by gallery) [ship name])
    :-  ~
    ?~  canvas  state
    =.  file.metadata.u.canvas  `file
    state(gallery (~(put by gallery) [ship name] u.canvas))
  ::
  ++  handle-unlock
    |=  name=@t
    ^-  (quip card _state)
    ?>  (team:title our.bowl src.bowl)
    ~&  >>  "[ unlock ] / TBA"
    ?:  &  `state
    ?>  (team:title our.bowl src.bowl)
    =/  =canvas  (~(got by gallery) [our.bowl name])
    =.  private.metadata.canvas  |
    :-  ~
    state(gallery (~(put by gallery) [our.bowl name] canvas))
  ::
  --
::
++  handle-canvas-update
  |=  act=canvas-update
  ^-  (quip card _state)
  |^
  ?-  -.act
    %load    (handle-load +.act)
    %paint   (handle-paint +.act)
  ==
  ::
  ++  handle-load
    |=  [name=@t =canvas]
    ^-  (quip card _state)
    ~&  >  "loading canvas..."
    :_  state(gallery (~(put by gallery) [[src.bowl name] canvas]))
    (send-frontend (canvas-view-response-to-json %load +<))
  ::
  ++  handle-paint
    |=  [location=@p name=@t strokes=(list stroke) who=@p]
    ^-  (quip card _state)
    ?:  =(who our.bowl)
      ::  we receive an update for a stroke we originaly sent; discard
      ::
      `state
    (process-remote-paint location name strokes)
  --
::
++  process-remote-paint
  |=  [location=@p name=@t strokes=(list stroke)]
  ^-  (quip card _state)
  ?>  ?=(^ strokes)
  =/  canvas=(unit canvas)  (~(get by gallery) [location name])
  ?~  canvas  `state
  ?.  =(-.i.strokes -.u.canvas)  `state
  |^
  =*  meta  metadata.u.canvas
  :-  (send-effects strokes)
  %_    state
      gallery
    %+  ~(put by gallery)
      [location name]
    ^-  ^canvas
    ?-  -.u.canvas
      %mesh  [%mesh (update-mesh mesh.u.canvas strokes) meta]
      %draw  [%draw (update-draw draw.u.canvas strokes) meta]
    ==
  ==
  ::
  ++  send-effects
    |=  strokes=(list stroke)
    ^-  (list card)
    ?.  (team:title our.bowl src.bowl)
      ::  stroke from a remote ship
      ::
      :-  [(send-paint-update name strokes src.bowl)]
      %-  send-frontend
      %-  canvas-view-response-to-json
      [%paint location name strokes]
    ::  stroke from frontend
    ::
    ?:  =(location our.bowl)
      [(send-paint-update name strokes our.bowl)]~
      :: =/  paint  !>([%paint location name strokes])
      :: :~  [%give %fact [/updates]~ %canvas-view paint]
      ::     [(send-paint-update name strokes src.bowl)]
      :: ==
    [(update-remote-canvas location name strokes)]~
  ::
  ++  update-mesh
    |=  [=mesh strokes=(list stroke)]
    ^-  ^mesh
    |-
    ?~  strokes  mesh
    ?>  ?=(%mesh -.i.strokes)
    %_    $
        strokes
      t.strokes
    ::
        mesh
      ?~  arc.i.strokes
        (~(del by mesh) id.i.strokes)
      (~(put by mesh) [id.i.strokes u.arc.i.strokes])
    ==
  ::
  ++  update-draw
    |=  [=draw strokes=(list stroke)]
    ^-  ^draw
    |-
    ?~  strokes  draw
    ?>  ?=([%draw *] i.strokes)
    %_  $
      draw     (snoc draw form.i.strokes)
      strokes  t.strokes
    ==
  --
::
++  extract-location
  |=  =path
  ^-  [ship @t]
  ?>  ?=([@t @t ~] path)
  :_  i.t.path
  (rash i.path ;~(pfix sig fed:ag))
::
++  subscribe
  |=  [=ship name=@t]
  ^-  card
  :*  %pass
      /subscribe/(scot %p ship)/(scot %tas name)
      %agent
      [ship %canvas]
      %watch
      /canvas/(scot %tas name)
  ==
::
++  poke-our
  |=  [app=term =cage]
  ^-  card
  [%pass / %agent [our.bowl app] %poke cage]
::
++  gall-scry
  |*  [=mold app=@tas =path]
  .^(mold %gx (weld /(scot %p our.bowl)/[app]/(scot %da now.bowl) path))
::
++  leave
  |=  [=ship name=@t]
  ^-  card
  :*  %pass
      /subscribe/(scot %p ship)/(scot %tas name)
      %agent
      [ship %canvas]
      %leave
      ~
  ==
::
++  send-init-canvas
  |=  name=@t
  ^-  (quip card _state)
  =/  canvas=(unit canvas)  (~(get by gallery) [our.bowl name])
  ?~  canvas  `state
  ~&  >  ['subscribing...' src.bowl private.metadata.u.canvas]
  ?:  private.metadata.u.canvas
    ~|([%subs-not-allowed name] !!)
  :_  state
  [%give %fact ~ %canvas-update !>([%load name u.canvas])]~
::
++  send-paint-update
  |=  [name=@t strokes=(list stroke) who=@p]
  ^-  card
  :*  %give
      %fact
      [/canvas/(scot %tas name)]~
      %canvas-update
      !>([%paint our.bowl name strokes who])
  ==
::
++  send-frontend
  |=  =json
  ^-  (list card)
  [%give %fact [/frontend]~ %json !>(json)]~
::
++  update-remote-canvas
  |=  [location=@p name=@t strokes=(list stroke)]
  ^-  card
  ?>  ?=(^ strokes)
  :*  %pass
      [%paint name ~]
      %agent
      [location %canvas]
      %poke
      %canvas-action
      !>([%paint location name strokes])
  ==
--
