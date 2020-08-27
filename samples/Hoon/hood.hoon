/+  default-agent
/+  drum=hood-drum, helm=hood-helm, kiln=hood-kiln
|%
+$  state
  $:  %9
      drum=state:drum
      helm=state:helm
      kiln=state:kiln
  ==
+$  any-state
  $%  state
      [ver=?(%1 %2 %3 %4 %5 %6) lac=(map @tas fin-any-state)]
      [%7 drum=state:drum helm=state:helm kiln=state:kiln]
      [%8 drum=state:drum helm=state:helm kiln=state:kiln]
  ==
+$  any-state-tuple
  $:  drum=any-state:drum
      helm=any-state:helm
      kiln=any-state:kiln
  ==
+$  fin-any-state
  $%  [%drum any-state:drum]
      [%helm any-state:helm]
      [%kiln any-state:kiln]
      [%write *]  ::  gets deleted
  ==
--
^-  agent:gall
=|  =state
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    drum-core  (drum bowl drum.state)
    helm-core  (helm bowl helm.state)
    kiln-core  (kiln bowl kiln.state)
::
++  on-fail   on-fail:def
++  on-init
  ^-  step:agent:gall
  =^  d  drum.state  on-init:drum-core
  [d this]
::
++  on-leave  on-leave:def
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
    [* %kiln *]  (on-peek:kiln-core path)
  ==
::
++  on-save   !>(state)
++  on-load
  |=  =old-state=vase
  ^-  step:agent:gall
  =+  !<(old=any-state old-state-vase)
  =/  tup=any-state-tuple
    ?+    -.old  +.old
        ?(%1 %2 %3 %4 %5 %6)
      :*  =-(?>(?=(%drum -<) ->) (~(got by lac.old) %drum))
          =-(?>(?=(%helm -<) ->) (~(got by lac.old) %helm))
          =-(?>(?=(%kiln -<) ->) (~(got by lac.old) %kiln))
      ==
    ==
  =^  d  drum.state  (on-load:drum-core -.old drum.tup)
  =^  h  helm.state  (on-load:helm-core -.old helm.tup)
  =^  k  kiln.state  (on-load:kiln-core -.old kiln.tup)
  [:(weld d h k) this]
::
++  on-poke
  |=  [=mark =vase]
  ^-  step:agent:gall
  |^
  =/  fin  (end 3 4 mark)
  ?:  =(%drum fin)  poke-drum
  ?:  =(%helm fin)  poke-helm
  ?:  =(%kiln fin)  poke-kiln
  ::
  ?+  mark  (on-poke:def mark vase)
    %atom            poke-helm(mark %helm-atom)
    %dill-belt       poke-drum(mark %drum-dill-belt)
    %dill-blit       poke-drum(mark %drum-dill-blit)
    %hood-sync       poke-kiln(mark %kiln-sync)
    %write-sec-atom  poke-helm(mark %helm-write-sec-atom)
  ==
  ++  poke-drum  =^(c drum.state (poke:drum-core mark vase) [c this])
  ++  poke-helm  =^(c helm.state (poke:helm-core mark vase) [c this])
  ++  poke-kiln  =^(c kiln.state (poke:kiln-core mark vase) [c this])
  --
::
++  on-watch
  |=  =path
  ^-  step:agent:gall
  ?+  path  (on-watch:def +<)
    [%drum *]  =^(c drum.state (peer:drum-core +<) [c this])
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  step:agent:gall
  ?+  wire  ~|([%hood-bad-wire wire] !!)
    [%drum *]  =^(c drum.state (take-agent:drum-core +<) [c this])
    [%helm *]  =^(c helm.state (take-agent:helm-core +<) [c this])
    [%kiln *]  =^(c kiln.state (take-agent:kiln-core +<) [c this])
  ==
::  TODO: symmetry between adding and stripping wire prefixes
::
++  on-arvo
  |=  [=wire syn=sign-arvo]
  ^-  step:agent:gall
  ?+  wire  ~|([%hood-bad-wire wire] !!)
    [%drum *]  =^(c drum.state (take-arvo:drum-core t.wire syn) [c this])
    [%helm *]  =^(c helm.state (take-arvo:helm-core t.wire syn) [c this])
    [%kiln *]  =^(c kiln.state (take-arvo:kiln-core t.wire syn) [c this])
  ==
--
