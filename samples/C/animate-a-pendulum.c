#include <stdlib.h>
#include <math.h>
#include <GL/glut.h>
#include <GL/gl.h>
#include <sys/time.h>

#define length 5
#define g 9.8
double alpha, accl, omega = 0, E;
struct timeval tv;

double elappsed() {
	struct timeval now;
	gettimeofday(&now, 0);
	int ret = (now.tv_sec - tv.tv_sec) * 1000000
		+ now.tv_usec - tv.tv_usec;
	tv = now;
	return ret / 1.e6;
}

void resize(int w, int h)
{
	glViewport(0, 0, w, h);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glOrtho(0, w, h, 0, -1, 1);
}

void render()
{
	double x = 320 + 300 * sin(alpha), y = 300 * cos(alpha);
	resize(640, 320);
 	glClear(GL_COLOR_BUFFER_BIT);

	glBegin(GL_LINES);
	glVertex2d(320, 0);
	glVertex2d(x, y);
	glEnd();
	glFlush();

	double us = elappsed();
	alpha += (omega + us * accl / 2) * us;
	omega += accl * us;

	/* don't let precision error go out of hand */
	if (length * g * (1 - cos(alpha)) >= E) {
		alpha = (alpha < 0 ? -1 : 1) * acos(1 - E / length / g);
		omega = 0;
	}
	accl = -g / length * sin(alpha);
}

void init_gfx(int *c, char **v)
{
	glutInit(c, v);
	glutInitDisplayMode(GLUT_RGB);
	glutInitWindowSize(640, 320);
	glutIdleFunc(render);
	glutCreateWindow("Pendulum");
}

int main(int c, char **v)
{
	alpha = 4 * atan2(1, 1) / 2.1;
	E = length * g * (1 - cos(alpha));

	accl = -g / length * sin(alpha);
	omega = 0;

	gettimeofday(&tv, 0);
	init_gfx(&c, v);
	glutMainLoop();
	return 0;
}
