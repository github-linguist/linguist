require 'rubygems'
require 'gl'
require 'glut'

W, H = 320, 240
SIZE = W * H

Glut.glutInit ARGV
Glut.glutInitWindowSize W, H

Glut.glutIdleFunc lambda {
  i = Time.now
  noise = (1..SIZE).map { rand > 0.5 ? 0xFFFFFFFF : 0xFF000000 }.pack("I*")

  Gl.glClear Gl::GL_COLOR_BUFFER_BIT
  Gl.glDrawPixels W, H, Gl::GL_RGBA, Gl::GL_UNSIGNED_BYTE, noise
  Gl.glFlush

  puts 1.0 / (Time.now - i)
}

Glut.glutCreateWindow "noise"
Glut.glutMainLoop
