#import "GLView.h"

#import "App.h"

const float TIMER_UPDATE_HZ = 1000;

/*	------------------------------------------------
	Notes on Setting Up a Custom NSApplication class
	------------------------------------------------
	
	1) in MainMenu.nib the File's Owner custom class is set to App
	2) In Project's Application settings the Principal class is set to App
*/

@implementation App

- (id) init
{
	self = [super init];

	[self setDelegate: self]; // (to get NSApplication notifications)
		
	return self;
}

- (void) sendEvent: (NSEvent*) theEvent
{
	// pass events up to the Sys class. If they don't handle it, send it up to NSApplication:

	if (!sys.HandleEvent(theEvent))
		[super sendEvent: theEvent];
}

- (void) applicationWillFinishLaunching: (NSNotification*) notification
{
}

- (void) applicationDidFinishLaunching: (NSNotification*) notification
{
	// Init the Sys class (which will start the event timer):

	sys.Init(TIMER_UPDATE_HZ, self, @selector(timerUpdate:));
}

- (void) applicationWillTerminate: (NSNotification*) notification
{
	sys.Reset();
}


/* The timerUpdate: method is called by an NSTimer (instantiated in the Sys class) */

- (void) timerUpdate : (id) object
{
	sys.Process();
	
	[glView timerUpdate];
}


/* IBActions: In Interface Builder the UI controls were wired to target 'File's Owner', which is us */

- (IBAction) modeSelect: (id) sender
{
	[glView setMode: [sender intValue]];
}

- (IBAction) alphaSlider: (id) sender
{
	[glView setAlpha: [sender floatValue]];
}

@end