#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>

int main()
{
  Display *d;
  XEvent event;

  d = XOpenDisplay(NULL);
  if ( d != NULL ) {
                /* or simply XK_F7 should work too */
    XGrabKey(d, XKeysymToKeycode(d, XStringToKeysym("F7")),
	     Mod1Mask, /* normally it's Alt */
	     DefaultRootWindow(d), True, GrabModeAsync, GrabModeAsync);
    XGrabKey(d, XKeysymToKeycode(d, XStringToKeysym("F6")),
	     Mod1Mask,
	     DefaultRootWindow(d), True, GrabModeAsync, GrabModeAsync);

    for(;;)
    {
      XNextEvent(d, &event);
      if ( event.type == KeyPress ) {
	KeySym s = XLookupKeysym(&event.xkey, 0);
	if ( s == XK_F7 ) {
	  printf("something's happened\n");
	} else if ( s == XK_F6 ) {
	  break;
	}
      }
    }

    XUngrabKey(d, XKeysymToKeycode(d, XStringToKeysym("F7")), Mod1Mask, DefaultRootWindow(d));
    XUngrabKey(d, XKeysymToKeycode(d, XStringToKeysym("F6")), Mod1Mask, DefaultRootWindow(d));
  }
  return EXIT_SUCCESS;
}
