int main() {
	GLUE.init(([
		"fullscreen": 0,
		"resolution": ({ 640, 480 }),
	]));
	
	while (1) {
		GL.glViewport(0, 0, 640, 480);
		GL.glMatrixMode(GL.GL_PROJECTION);
		GL.glLoadIdentity();
		GL.glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0);
		GL.glMatrixMode(GL.GL_MODELVIEW);
		
		GL.glClearColor(0.3,0.3,0.3,0.0);
		GL.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
		
		GL.glShadeModel(GL.GL_SMOOTH);
		
		GL.glLoadIdentity();
		GL.glTranslate(-15.0, -15.0, 0.0);
		
		GL.glBegin(GL.GL_TRIANGLES);
		GL.glColor(1.0, 0.0, 0.0);
		GL.glVertex(0.0, 0.0);
		GL.glColor(0.0, 1.0, 0.0);
		GL.glVertex(30.0, 0.0);
		GL.glColor(0.0, 0.0, 1.0);
		GL.glVertex(0.0, 30.0);
		GL.glEnd();
		
		GL.glFlush();
		
		GLUE.swap_buffers();
		Pike.DefaultBackend(0.0);
		sleep(0.01);
	}
}
