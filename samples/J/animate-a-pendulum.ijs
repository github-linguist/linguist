require 'gl2 trig'
coinsert 'jgl2'

DT   =: %30      NB. seconds
ANGLE=: 0.25p1   NB. radians
L    =: 1        NB. metres
G    =: 9.80665  NB. ms_2
VEL  =: 0        NB. ms_1

PEND=: noun define
pc pend;pn "Pendulum";
xywh 0 0 320 200;cc isi isigraph rightmove bottommove;
pas 0 0;pcenter;
rem form end;
)

pend_run      =: verb def ' wd PEND,'';pshow;timer '',":DT * 1000 '
pend_close    =: verb def ' wd ''timer 0; pclose'' '
pend_isi_paint=: verb def ' drawPendulum ANGLE '

sys_timer_z_=: verb define
  recalcAngle ''
  wd 'psel pend; setinvalid isi'
)

recalcAngle=: verb define
  accel=. - (G % L) * sin ANGLE
  VEL  =: VEL + accel * DT
  ANGLE=: ANGLE + VEL * DT
)

drawPendulum=: verb define
  width=. {. glqwh''
  ps=. (-: width) , 40
  pe=. ps + 280 <.@* (cos , sin) 0.5p1 + y    NB. adjust orientation
  glrgb 91 91 91
  glbrush''
  gllines ps , pe
  glellipse (,~ ps - -:) 40 15
  glellipse (,~ pe - -:) 20 20
  glrect 0 0 ,width, 40
)

pend_run''                                    NB. run animation
