require 'rubygems'
require 'gl'
require 'glut'

include Gl
include Glut

paint = lambda do
  glClearColor(0.3,0.3,0.3,0.0)
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

  glShadeModel(GL_SMOOTH)

  glLoadIdentity
  glTranslatef(-15.0, -15.0, 0.0)

  glBegin(GL_TRIANGLES)
    glColor3f(1.0, 0.0, 0.0)
    glVertex2f(0.0, 0.0)
    glColor3f(0.0, 1.0, 0.0)
    glVertex2f(30.0, 0.0)
    glColor3f(0.0, 0.0, 1.0)
    glVertex2f(0.0, 30.0)
  glEnd

  glFlush
end

reshape = lambda do |width, height|
  glViewport(0, 0, width, height)
  glMatrixMode(GL_PROJECTION)
  glLoadIdentity
  glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)
  glMatrixMode(GL_MODELVIEW)
end

glutInit
glutInitWindowSize(640, 480)
glutCreateWindow("Triangle")

glutDisplayFunc(paint)
glutReshapeFunc(reshape)

glutMainLoop
