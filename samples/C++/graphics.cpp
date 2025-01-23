// License - https://github.com/TurtleP/Flask/blob/master/LICENSE

#include <shared.h>

int currentR = 0xFF;
int currentG = 0xFF;
int currentB = 0xFF;
int currentA = 0xFF;

int currentScreen = GFX_BOTTOM;

float transX = 0;
float transY = 0;
bool isPushed = false;

u32 getCurrentColor() 
{
	return RGBA8(currentR, currentG, currentB, currentA);
}

void setColor(int r, int g, int b)
{
	currentR = r;
	currentG = g;
	currentB = b;
	currentA = currentA;
}

void setColor(int r, int g, int b, int a)
{
	currentR = r;
	currentG = g;
	currentB = b;
	currentA = a;
}

void setScreen(int screen)
{
	currentScreen = screen;
}

int getCurrentScreen()
{
	return currentScreen;
}

void screenShot() //for showing stuff being done
{
	FILE * topScreen = fopen("sdmc:/framebuffer_top.rgb", "w+");

	fwrite(gfxGetFramebuffer(GFX_TOP, GFX_LEFT, NULL, NULL), 288000, 1, topScreen);

	fclose(topScreen);

	FILE * bottomScreen = fopen("sdmc:/framebuffer_bottom.rgb", "w+");;

	fwrite(gfxGetFramebuffer(GFX_BOTTOM, GFX_LEFT, NULL, NULL), 230400, 1, bottomScreen);

	fclose(bottomScreen);
}

void translateCoords(float * x, float * y) {
	if (isPushed) 
	{
		*x += transX;
		*y += transY;
	}
}

void translate(float dx, float dy)
{
	if (sf2d_get_current_screen() == getCurrentScreen()) 
	{
		transX = transX + dx;
		transY = transY + dy;
	}
}

void push()
{
	if (sf2d_get_current_screen() == getCurrentScreen()) 
	{
		isPushed = true;
	}
}

void pop()
{
	if (sf2d_get_current_screen() == getCurrentScreen()) 
	{
		transX = 0;
		transY = 0;
		isPushed = false;
	}
}

void setScissor(u32 x, u32 y, u32 width, u32 height)
{
	if (sf2d_get_current_screen() == getCurrentScreen()) 
	{
		GPU_SCISSORMODE mode = GPU_SCISSOR_NORMAL;

		if (!x && !y && !width && !height) {
			mode = GPU_SCISSOR_DISABLE;
		}

		sf2d_set_scissor_test(mode, x, y, width, height);
	}
}