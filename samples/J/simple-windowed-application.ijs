SIMPLEAPP=: noun define
pc simpleApp;
xywh 131 11 44 12;cc inc button;cn "Click me";
xywh 7 10 115 11;cc shownText static;cn "There have been no clicks yet.";
pas 6 6;pcenter;
rem form end;
)

simpleApp_run=: verb define
  wd SIMPLEAPP
  simpleApp_accum=: 0   NB. initialize accumulator
  wd 'pshow;'
)

simpleApp_inc_button=: verb define
  wd 'set shownText *','Button-use count:  ',": simpleApp_accum=: >: simpleApp_accum
)

simpleApp_close=: wd bind 'pclose'
simpleApp_cancel=: simpleApp_close

simpleApp_run''
