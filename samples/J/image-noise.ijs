coclass'example'
(coinsert[require)'jzopengl'

P=: 0 : 0
pc p nosize;
xywh 0 0 160 120;cc c isigraph opengl;
pas 0 0;pcenter;
rem form end;
 pshow;
 timer 1;
)

timestamp=: (6!:8'') %~ 6!:9

create=:3 :0
  ogl=:''conew'jzopengl'
  frames=:0
  start=: timestamp''
  sys_timer_base_=: ''1 :('p_c_paint_',(;coname''),'_')
  wd P
)

p_run=: 3 : 0
  ''conew'example'
)

destroy=:3 :0
  end=:timestamp''
  smoutput 'frames per second: ',":frames%end-start
  wd 'timer 0'
  destroy__ogl''
  wd'pclose'
  codestroy''
)

p_close=: destroy

p_c_paint=: 3 : 0
  rc__ogl''
  glClear GL_COLOR_BUFFER_BIT
  glBegin GL_POINTS
    glVertex _1+2*53050 2?@$ 0
  glEnd''
  show__ogl''
  frames=:frames+1
)

p_run''
