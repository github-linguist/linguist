SIMPLEGUI=: noun define
pc simpleGui;
xywh 136 39 44 12;cc accept button;cn "Accept";
xywh 0 14 60 11;cc IntegerLabel static ss_right;cn "Enter an integer";
xywh 65 13 60 12;cc integer edit;
xywh 0 39 60 11;cc TextLabel static ss_right;cn "Enter text";
xywh 64 38 60 12;cc text edit;
pas 6 6;pcenter;
rem form end;
)

simpleGui_run=: verb define
  wd SIMPLEGUI
  wd 'set integer *', ": 75000
  wd 'pshow;'
)

simpleGui_accept_button=: verb define
  ttxt=. text
  tint=. _". integer       NB. invalid integers assigned value _
  if. tint ~: 75000  do.
    wdinfo 'Integer entered was not 75000.'
  else.
    simpleGui_close ''
    'simpleGui_text simpleGui_integer'=: ttxt;tint
  end.
)

simpleGui_close=: wd bind 'pclose'
simpleGui_cancel=: simpleGui_close

simpleGui_run''
