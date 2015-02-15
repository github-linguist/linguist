open GL
open Glut

let display() =
  glClearColor 0.3 0.3 0.3 0.0;
  glClear[GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];

  glShadeModel GL_SMOOTH;

  glLoadIdentity();
  glTranslate (-15.0) (-15.0) (0.0);

  glBegin GL_TRIANGLES;
  glColor3 1.0 0.0 0.0;
  glVertex2 0.0 0.0;
  glColor3 0.0 1.0 0.0;
  glVertex2 30.0 0.0;
  glColor3 0.0 0.0 1.0;
  glVertex2 0.0 30.0;
  glEnd();

  glFlush();
;;

let reshape ~width ~height =
  glViewport 0 0 width height;
  glMatrixMode GL_PROJECTION;
  glLoadIdentity();
  glOrtho(-30.0) 30.0 (-30.0) 30.0 (-30.0) 30.0;
  glMatrixMode GL_MODELVIEW;
;;

let () =
  ignore(glutInit Sys.argv);
  glutInitWindowSize 640 480;
  ignore(glutCreateWindow "Triangle");

  glutDisplayFunc ~display;
  glutReshapeFunc ~reshape;

  glutMainLoop();
;;
