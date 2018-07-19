:- module opengl.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, glut, glut.callback, glut.window, mogl.

main(!IO) :-
   glut.init_window_size(640, 480, !IO),
   glut.window.create("Triangle", !IO),
   glut.callback.display_func(opengl.paint, !IO),
   glut.callback.reshape_func(opengl.reshape, !IO),
   glut.main_loop(!IO).

:- pred paint(io::di, io::uo) is det.

paint(!IO) :-
   mogl.clear_color(0.3, 0.3, 0.3 , 0.0, !IO),
   mogl.clear([color, depth], !IO),

   mogl.shade_model(smooth, !IO),

   mogl.load_identity(!IO),
   mogl.translate(-15.0, -15.0, 0.0, !IO),

   mogl.begin(triangles, !IO),
       mogl.color3(1.0, 0.0, 0.0, !IO),
       mogl.vertex2(0.0, 0.0, !IO),
       mogl.color3(0.0, 1.0, 0.0, !IO),
       mogl.vertex2(30.0, 0.0, !IO),
       mogl.color3(0.0, 0.0, 1.0, !IO),
       mogl.vertex2(0.0, 30.0, !IO),
   mogl.end(!IO),

   mogl.flush(!IO).

:- pred reshape(int::in, int::in, io::di, io::uo) is det.

reshape(Width, Height, !IO) :-
  mogl.viewport(0, 0, Width, Height, !IO),
  mogl.matrix_mode(projection, !IO),
  mogl.load_identity(!IO),
  mogl.ortho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0, !IO),
  mogl.matrix_mode(modelview, !IO).
