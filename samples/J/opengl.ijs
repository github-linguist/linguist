coclass 'example'
(coinsert[require) 'jzopengl'

create=:3 :0
  ogl=: ''conew'jzopengl'
  wd 'pc p;cc c isigraph opengl rightmove bottommove;pas 0 0;pshow;'
)

p_close=: destroy=:3 :0
  destroy__ogl''
  wd'pclose'
  codestroy''
)

corner=:4 :0
  glColor3d x
  glVertex2d y
)

p_c_paint=:3 :0
  rc__ogl''
  glClear GL_COLOR_BUFFER_BIT
  glBegin GL_TRIANGLES
    1 0 0 corner 0 0-0.5
    0 1 0 corner 1 0-0.5
    0 0 1 corner 0 1-0.5
  glEnd''
  show__ogl''
)

conew~'example'
