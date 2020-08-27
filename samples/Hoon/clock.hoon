::  clock: deprecated, should be removed
::
/+  *server, default-agent, verb, dbug
=,  format
::
|%
::
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
  ==
+$  state-zero  [%0 data=json]
--
%+  verb  |
%-  agent:dbug
=|  state-zero
=*  state  -
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  on-init:def
++  on-save   !>(%3)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  :_  this
  [%pass / %arvo %e %disconnect [~ /'~clock']]~
::
++  on-poke  on-poke:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
