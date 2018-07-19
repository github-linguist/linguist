module opengl_sample; // file name + directory
import dglut.core, dglut.window, dglut.opengl;

void main() {
  with (new Canvas) {
    setName("Triangle");
    map;

    onResize = (Canvas self) { // A delegate literal that takes a parameter.
      with (self) glViewport(0, 0, width, height);
      MatrixMode.Projection.Identity; // For functions without parameters, the () can be omitted.
      glOrtho(-30, 30, -30, 30, -30, 30);
      MatrixMode.Modelview;
    };

    onDisplay=(Canvas self) {
      scope(exit) self.swap; // Scope guards ease exception-safe programming
      glClearColor(0.3f, 0.3f, 0.3f, 0f);
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      glLoadIdentity;
      // A convenience wrapper around glTranslatef. Supports numbers, arrays and vectors.
      Translate(-15, -15, 0);
      // This is a delegate literal as well. Triangles is a wrapper around glBegin and glEnd.
      Triangles = {
        Color(1f, 0f, 0f); Vertex(0, 0);
        Color(0f, 1f, 0f); Vertex(30, 0);
        Color(0f, 0f, 1f); Vertex(0, 30);
      };
    };
  }
  loop;
}
