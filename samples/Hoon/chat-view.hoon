::  chat-view: sets up chat JS client, paginates data, and combines commands
::  into semantic actions for the UI
::
/-  *permission-store,
    *permission-hook,
    *group,
    *invite-store,
    *metadata-store,
    group-hook,
    *permission-group-hook,
    *chat-hook,
    *metadata-hook,
    hook=chat-hook,
    contact-view,
    pull-hook
/+  *server, default-agent, verb, dbug,
    store=chat-store,
    view=chat-view,
    group-store,
    grpl=group,
    resource,
    mdl=metadata
::
~%  %chat-view-top  ..is  ~
|%
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0
  $:  %0
      ~
  ==
+$  poke
  $%  [%chat-action action:store]
      [%group-action action:group-store]
      [%chat-hook-action action:hook]
      [%permission-hook-action permission-hook-action]
      [%permission-group-hook-action permission-group-hook-action]
  ==
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
=<
  ~%  %chat-view-agent-core  ..poke-handle-http-request  ~
  |_  bol=bowl:gall
  +*  this       .
      chat-core  +>
      cc         ~(. chat-core bol)
      def        ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    :~  :*  %pass  /srv  %agent  [our.bol %file-server]
            %poke  %file-server-action
            !>([%serve-dir /'~chat' /app/landscape %.n])
        ==
      [%pass / %arvo %e %connect [~ /'chat-view'] %chat-view]
      [%pass /updates %agent [our.bol %chat-store] %watch /updates]
    ==
  ::
  ++  on-poke
    ~/  %chat-view-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bol src.bol)
    ?+    mark  (on-poke:def mark vase)
        %handle-http-request
      =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
      :_  this
      %+  give-simple-payload:app  eyre-id
      %+  require-authorization:app  inbound-request
      poke-handle-http-request:cc
    ::
        %json
      :_  this
      (poke-chat-view-action:cc (action:dejs:view !<(json vase)))
    ::
        %chat-view-action
      :_  this
      (poke-chat-view-action:cc !<(action:view vase))
    ==
  ::
  ++  on-watch
    ~/  %chat-view-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bol src.bol)
    |^
    ?:  ?=([%http-response *] path)
      [~ this]
    ?:  =(/primary path)
      ::  create inbox with 20 messages max per mailbox and send that along
      ::  then quit the subscription
      :_  this
      [%give %fact ~ %json !>((update:enjs:store [%initial truncated-inbox]))]~
    (on-watch:def path)
    ::
    ++  message-limit  20
    ::
    ++  truncated-inbox
      ^-  inbox:store
      =/  =inbox:store
        =/  our  (scot %p our.bol)
        =/  now  (scot %da now.bol)
        .^(inbox:store %gx /[our]/chat-store/[now]/all/noun)
      %-  ~(run by inbox)
      |=  =mailbox:store
      ^-  mailbox:store
      [config.mailbox (scag message-limit envelopes.mailbox)]
    --
  ::
  ++  on-agent
    ~/  %chat-view-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+    -.sign  (on-agent:def wire sign)
        %poke-ack
      ?.  ?=([%join-group @ @ @ @ @ ~] wire)
        (on-agent:def wire sign)
      ?^  p.sign
        (on-agent:def wire sign)
      =/  =ship
        (slav %p i.t.wire)
      =/  ask-history=?
        =('y' i.t.t.wire)
      =/  rid=resource
        (de-path:resource t.t.t.wire)
      :_  this
      (joined-group:cc rid ship ask-history)
    ::
        %kick
      :_  this
      [%pass / %agent [our.bol %chat-store] %watch /updates]~
    ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %chat-update
        :_  this
        (diff-chat-update:cc !<(update:store q.cage.sign))
      ==
    ==
  ::
  ++  on-arvo
    ~/  %chat-view-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?:  ?=(%bound +<.sign-arvo)  [~ this]
    (on-arvo:def wire sign-arvo)
  ::
  ++  on-save  !>(state)
  ++  on-load  
    |=  old-vase=vase
    ^-  (quip card _this)
    =/  old  ((soft state-0) q.old-vase)
    ?^  old  [~ this]
    :_  this(state [%0 ~])
    :~  [%pass / %arvo %e %disconnect [~ /'~chat']]
        [%pass / %arvo %e %connect [~ /'chat-view'] %chat-view]
        :*  %pass  /srv  %agent  [our.bol %file-server]
            %poke  %file-server-action
            !>([%serve-dir /'~chat' /app/landscape %.n])
        ==
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-fail   on-fail:def
  --
::
::
~%  %chat-view-library  ..card  ~
|_  bol=bowl:gall
++  grp  ~(. grpl bol)
++  md   ~(. mdl bol)
::
++  poke-handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  =+  url=(parse-request-line url.request.inbound-request)
  ?+  site.url  not-found:gen
      [%'chat-view' %paginate @t @t *]
    =/  start  (need (rush i.t.t.site.url dem))
    =/  end  (need (rush i.t.t.t.site.url dem))
    =/  pax  t.t.t.t.site.url
    =/  envelopes  (envelope-scry [(scot %ud start) (scot %ud end) pax])
    %-  json-response:gen
    %-  json-to-octs
    %-  update:enjs:store
    [%messages pax start end envelopes]
  ==
::
++  poke-json
  |=  jon=json
  ^-  (list card)
  ?>  (team:title our.bol src.bol)
  (poke-chat-view-action (action:dejs:view jon))
::
++  poke-chat-view-action
  |=  act=action:view
  ^-  (list card)
  |^
  ?>  (team:title our.bol src.bol)
  ?-  -.act
      %create
    ?>  ?=(^ app-path.act)
    ?>  ?|  =(+:group-path.act app-path.act) 
            =(~(tap in members.act) ~) 
        ==
    ?^  (chat-scry app-path.act)
      ~&  %chat-already-exists
      ~
    %-  zing
    :~  (create-chat app-path.act allow-history.act)
        %-  create-group
        :*  group-path.act
            app-path.act
            policy.act
            members.act
            title.act
            description.act
            managed.act
        ==
        (create-metadata title.act description.act group-path.act app-path.act)
    ==
  ::
      %delete
    ?>  ?=(^ app-path.act)
    ::  always just delete the chat from chat-store
    ::
    :+  (chat-hook-poke [%remove app-path.act])
      (chat-poke [%delete app-path.act])
    ::  if we still have metadata for the chat, remove it, and the associated
    ::  group if it's unmanaged.
    ::
    ::    we aren't guaranteed to have metadata: the chat might have been
    ::    deleted by the host, which pushes metadata deletion down to us.
    ::
    =/  maybe-group-path
      (maybe-group-from-chat app-path.act)
    ?~  maybe-group-path
      ~
    =*  group-path  u.maybe-group-path
    =/  rid=resource
      (de-path:resource group-path)
    =/  maybe-group
      (scry-group:grp rid)
    =/  hidden
      ?~  maybe-group
        %.n
      hidden.u.maybe-group
    %-  zing
    :~  ?.  (is-creator group-path %chat app-path.act)
          ~
        [(metadata-poke [%remove group-path [%chat app-path.act]])]~
      ::
        ?.  hidden
          ~
        :~  (group-proxy-poke %remove-members rid (sy our.bol ~))
            (group-poke [%remove-group rid ~])
            (metadata-hook-poke [%remove group-path])
            (metadata-store-poke [%remove group-path [%chat app-path.act]])
        ==
    ==
  ::
      %invite
    =/  =group-path
      (need (maybe-group-from-chat app-path.act))
    =/  rid=resource
      (de-path:resource group-path)
    =/  =group
      (need (scry-group:grp rid))
    ?>  ?=(%invite -.policy.group)
    :-  (group-poke %change-policy rid %invite %add-invites ships.act)
    %+  turn
      ~(tap in ships.act)
    |=  =ship
    (send-invite group-path app-path.act ship)
  ::
      %join
    =/  group-path
      (maybe-group-from-chat app-path.act)
    =/  group
      ?~  group-path
        ~
      (scry-group-path:grp u.group-path)
    ?:  &(?=(^ group) =(hidden.u.group %.n))
      ~[(chat-hook-poke %add-synced ship.act app-path.act ask-history.act)]
    =/  rid=resource
      (de-path:resource ship+app-path.act)
    =/  =cage
      :-  %group-update
      !>  ^-  action:group-store
      [%add-members rid (sy our.bol ~)]
    ::  we need this info in the wire to continue the flow after the
    ::  poke ack
    =/  =wire
      :-  %join-group
      [(scot %p ship.act) ?:(ask-history.act %y %n) ship+app-path.act]
    [%pass wire %agent [entity.rid %group-push-hook] %poke cage]~
  ::
      %groupify
    =*  app-path  app-path.act
    =/  group-path
      (snag 0 (groups-from-resource:md %chat app-path))
    =/  scry-pax=path
      /metadata/[(scot %t (spat group-path))]/chat/[(scot %t (spat app-path))]
    =/  =metadata
      (need (scry-for (unit metadata) %metadata-store scry-pax))
    =/  old-rid=resource
      (de-path:resource group-path)
    ?<  (is-managed:grp old-rid)
    ?~  existing.act
      ::  just create contacts object for group
      ~[(contact-view-poke %groupify old-rid title.metadata description.metadata)]
    ::  change associations
    =*  group-path  group-path.u.existing.act
    =/  rid=resource
      (de-path:resource group-path)
    =/  old-group=group
      (need (scry-group:grp old-rid))
    =/  =group
      (need (scry-group:grp rid))
    =/  ships=(set ship)
      (~(dif in members.old-group) members.group)
    :*  (metadata-store-poke %remove ship+app-path %chat app-path)
        (metadata-store-poke %add group-path [%chat app-path] metadata)
        (group-poke %remove-group old-rid ~)
        ?.  inclusive.u.existing.act
          ~
        :-  (group-poke %add-members rid ships)
        %+  turn
          ~(tap in ships)
        |=  =ship
        (send-invite group-path app-path ship)
    ==
  ==
  ::
  ++  create-chat
    |=  [=path history=?]
    ^-  (list card)
    :~  (chat-poke [%create path])
        (chat-hook-poke [%add-owned path history])
    ==
  ::
  ++  create-group
    |=  [=path app-path=path =policy ships=(set ship) title=@t desc=@t managed=?]
    ^-  (list card)
    ?^  (scry-group-path:grp path)  ~
    =/  rid=resource
      (de-path:resource path)
    ?>  =(our.bol entity.rid)
    ::  do not create a contacts object if this is unmanaged
    ::
    :-
      ?.  managed
        (group-poke %add-group rid policy %.y)
      (contact-view-poke %create name.rid policy title desc)
    %+  murn  ~(tap in ships)
    |=  =ship
    ^-  (unit card)
    ?:  =(ship our.bol)  ~
    `(send-invite path app-path ship)
  ::
  ++  create-metadata
    |=  [title=@t description=@t group-path=path app-path=path]
    ^-  (list card)
    =/  =metadata
      %*  .  *metadata
          title         title
          description   description
          date-created  now.bol
          creator
        (slav %p (snag 0 app-path))
      ==
    :~  (metadata-poke [%add group-path [%chat app-path] metadata])
        (metadata-hook-poke [%add-owned group-path])
    ==
  ::
  ++  contact-view-poke
    |=  act=contact-view-action:contact-view
    ^-  card
    [%pass / %agent [our.bol %contact-view] %poke %contact-view-action !>(act)]
  ::
  ++  metadata-poke
    |=  act=metadata-action
    ^-  card
    [%pass / %agent [our.bol %metadata-hook] %poke %metadata-action !>(act)]
  ::
  ++  metadata-store-poke
    |=  act=metadata-action
    ^-  card
    [%pass / %agent [our.bol %metadata-store] %poke %metadata-action !>(act)]
  ::
  ++  send-invite
    |=  [group-path=path app-path=path =ship]
    ^-  card
    =/  managed=?
      !=(ship+app-path group-path)
    =/  =invite
      :*  our.bol
          ?:(managed %contact-hook %chat-hook)
          ?:(managed group-path app-path)
          ship  ''
      ==
    =/  act=invite-action  [%invite ?:(managed /contacts /chat) (shaf %msg-uid eny.bol) invite]
    [%pass / %agent [our.bol %invite-hook] %poke %invite-action !>(act)]
  ::
  ++  chat-scry
    |=  pax=path
    ^-  (unit mailbox:store)
    =.  pax
      ;:  weld
        /(scot %p our.bol)/chat-store/(scot %da now.bol)/mailbox
        pax
        /noun
      ==
    .^((unit mailbox:store) %gx pax)
  ::
  ++  maybe-group-from-chat
    |=  app-path=path
    ^-  (unit path)
    ?.  .^(? %gu (scot %p our.bol) %metadata-store (scot %da now.bol) ~)
      ?:  ?=([@ ^] app-path)
        ~&  [%assuming-ported-legacy-chat app-path]
        `[%'~' app-path]
      ~&  [%weird-chat app-path]
      !!
    =/  resource-indices
      .^  (jug md-resource group-path)
        %gy
        (scot %p our.bol)
        %metadata-store
        (scot %da now.bol)
        /resource-indices
      ==
    =/  groups=(set path)
      %+  fall
        (~(get by resource-indices) [%chat app-path])
      *(set path)
    ?~  groups  ~
    `n.groups
  ::
  ++  group-from-chat
    (cork maybe-group-from-chat need)
  ::
  ++  is-managed
    |=  =path
    ^-  ?
    ?>  ?=(^ path)
    !=(i.path '~')
  ::
  ++  is-creator
    |=  [group-path=path app-name=@ta app-path=path]
    ^-  ?
    =/  meta=(unit metadata)
      .^  (unit metadata)
        %gx
        (scot %p our.bol)
        %metadata-store
        (scot %da now.bol)
        %metadata
        (scot %t (spat group-path))
        app-name
        (scot %t (spat app-path))
        /noun
      ==
    ?~  meta  !!
    =(our.bol creator.u.meta)
  --
::  +joined-group: Successfully joined unmanaged group, continue flow
::
++  joined-group
  |=  [rid=resource =ship ask-history=?]
  ^-  (list card)
  =/  =path
    (en-path:resource rid)
  ?>  ?=(^ path)
  :~  (group-pull-hook-poke %add ship rid)
      (chat-hook-poke %add-synced ship t.path ask-history)
      (metadata-hook-poke %add-synced ship path)
  ==
::
++  diff-chat-update
  |=  upd=update:store
  ^-  (list card)
  [%give %fact ~[/primary] %json !>((update:enjs:store upd))]~
::
::  +utilities
::
++  chat-poke
  |=  act=action:store
  ^-  card
  [%pass / %agent [our.bol %chat-store] %poke %chat-action !>(act)]
::
++  group-poke
  |=  upd=update:group-store
  ^-  card
  [%pass / %agent [our.bol %group-store] %poke %group-update !>(upd)]
++  group-pull-hook-poke
  |=  act=action:pull-hook
  ^-  card
  [%pass / %agent [our.bol %group-pull-hook] %poke %pull-hook-action !>(act)]
::
++  group-proxy-poke
  |=  act=action:group-store
  ^-  card
  [%pass / %agent [entity.resource.act %group-push-hook] %poke %group-update !>(act)]
::
++  permission-poke
  |=  act=permission-action
  ^-  card
  [%pass / %agent [our.bol %permission-store] %poke %permission-action !>(act)]
::
++  chat-hook-poke
  |=  act=action:hook
  ^-  card
  [%pass / %agent [our.bol %chat-hook] %poke %chat-hook-action !>(act)]
::
++  permission-hook-poke
  |=  act=permission-hook-action
  ^-  card
  :*  %pass  /  %agent  [our.bol %permission-hook]
      %poke  %permission-hook-action  !>(act)
  ==
::
++  perm-group-hook-poke
  |=  act=permission-group-hook-action
  ^-  card
  :*  %pass  /  %agent  [our.bol %permission-group-hook]
      %poke  %permission-group-hook-action  !>(act)
  ==
::
++  metadata-hook-poke
  |=  act=metadata-hook-action
  ^-  card
  :*  %pass  /  %agent
      [our.bol %metadata-hook]
      %poke  %metadata-hook-action
      !>(act)
  ==
::
++  envelope-scry
  |=  pax=path
  ^-  (list envelope:store)
  (scry-for (list envelope:store) %chat-store [%envelopes pax])
::

::
++  scry-for
  |*  [=mold app=term =path]
  .^  mold
    %gx
    (scot %p our.bol)
    app
    (scot %da now.bol)
    (snoc `^path`path %noun)
  ==
--
