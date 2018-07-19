coinsert'jgl2' [ require'gl2'

MESSAGE          =:  'Hello World! '
TIMER_INTERVAL   =:  0.5 * 1000                                            NB.  Milliseconds
DIRECTION        =:  -1                                                    NB.  Initial direction is right -->

ANIM             =:  noun define
  pc anim nomax nosize;pn "Basic Animation in J";
  xywh 1 1 174 24;cc isi isigraph rightmove bottommove;
  pas 0 0;pcenter;pshow;
)

anim_run         =:  verb def ' wd ANIM,''; timer '',":TIMER_INTERVAL '
sys_timer_z_     =:  verb def ' isiMsg MESSAGE=:  DIRECTION |. MESSAGE '   NB.  Rotate MESSAGE according to DIRECTION
anim_isi_mbldown =:  verb def ' DIRECTION=:  - DIRECTION '                 NB.  Reverse direction when user clicks
anim_close       =:  verb def ' wd ''timer 0; pclose; reset;'' '           NB.  Shut down timer

isiMsg           =:  verb define
  wd'psel anim'
  glclear ''                                                               NB.  Clear out old drawing
  glfont '"courier new" 36'
  gltext y
  glpaint ''                                                               NB.  Copy to screen
)

anim_run ''
