#import "PTPreferences.h"

#define DEFAULTS_PATH @"/Library/PreferenceBundles/PowerTapPrefs.bundle/Defaults.plist"
#define USER_PREFS_PATH @"/User/Library/Preferences/com.dpkgdan.powertap.plist"

@implementation PTPreferences

static void reconcilePreferences(NSMutableDictionary *defaults, NSMutableDictionary *customPrefs,
								 NSString *preferenceSpecifier, NSString *entryToReplace)
{
	NSArray *defaultModes = [defaults allKeys]; //Array of modes listed in defaults

	for (int i = 0; i < [defaultModes count]; i++)
	{
		NSString *mode = [defaultModes objectAtIndex: i];
		NSString *customPrefsKey = [mode stringByAppendingString: preferenceSpecifier]; //Get key for given mode using
																						//preference specifier
		
		id preferenceObject = [customPrefs objectForKey: customPrefsKey];
		if (preferenceObject && !(([preferenceObject isKindOfClass: [NSString class]]) 
			&& ([preferenceObject isEqualToString: @""])))
			//Don't replace blank strings! Return to the placeholder (default)
		{	
			[[defaults objectForKey: mode] setObject: preferenceObject forKey: entryToReplace];	
		}
	}
}

- (PTPreferences*)init
{
	if ((self = [super init]))
	{
		NSMutableDictionary *defaults = [[NSMutableDictionary alloc] initWithContentsOfFile: DEFAULTS_PATH];
		NSMutableDictionary *customPrefs = [[NSMutableDictionary alloc] initWithContentsOfFile: USER_PREFS_PATH];
		if (customPrefs)
		{
			reconcilePreferences(defaults, customPrefs, @"-Toggle", @"enabled");
			reconcilePreferences(defaults, customPrefs, @"-String", @"string");
		}
		_preferences = [[NSDictionary alloc] initWithDictionary: defaults];
		_modes = [_preferences allKeys];
	}
	return self;	
}

- (NSString*)modeForIndex:(int)index
{
	for (int i = 0; i < [self.modes count]; i++)
	{
		int loopingIndex = [[self valueForSpecifier: @"index" mode: [self.modes objectAtIndex: i]] intValue];
		if (loopingIndex == index)
			return [self.modes objectAtIndex: i];
	}
	return nil;
}

- (id)valueForSpecifier:(NSString*)specifier mode:(NSString*)mode
{
	return [[_preferences objectForKey: mode] objectForKey: specifier];
}

- (UIImage*)iconForMode:(NSString*)mode
{
	UIImage *icon = nil;
	if ([mode isEqualToString: @"PowerDown"])
		icon = [UIImage imageNamed: @"PowerDownKnob"];
	else
	{
		NSString *pathToIcon = [self valueForSpecifier: @"icon" mode: mode];
		icon = [UIImage imageWithContentsOfFile: pathToIcon];
	}
	return icon;
}

- (UIColor*)tintColorForMode:(NSString*)mode
{
	NSString *colorString = [self valueForSpecifier: @"color" mode: mode];
	
	if ([colorString isEqualToString: @"red"])
		return [UIColor redColor];
	else if ([colorString isEqualToString: @"orange"])
		return [UIColor orangeColor];
	else if ([colorString isEqualToString: @"cyan"])
		return [UIColor cyanColor];
	else if ([colorString isEqualToString: @"purple"])
		return [UIColor purpleColor];
	else if ([colorString isEqualToString: @"gray"])
		return [UIColor grayColor];
	else if ([colorString isEqualToString: @"black"])
		return [UIColor blackColor];
	else if ([colorString isEqualToString: @"lightGray"])
		return [UIColor lightGrayColor];
	else if ([colorString isEqualToString: @"blue"])
		return [UIColor blueColor];
	else if ([colorString isEqualToString: @"magenta"])
		return [UIColor magentaColor];
	else if ([colorString isEqualToString: @"brown"])
		return [UIColor brownColor];
	else if ([colorString isEqualToString: @"green"])
		return [UIColor greenColor];
	else
		return nil;
}

- (NSArray*)trackTexts
{
	NSMutableArray *_trackTexts = [NSMutableArray new];
	
	for (int i = 0; i < [self.modes count]; i++)
		[_trackTexts addObject: [self valueForSpecifier: @"string" mode: [self modeForIndex: i]]];
	
	return [[NSArray alloc] initWithArray: _trackTexts];
}

- (void)setPowerDownTrackText:(NSString*)trackText
{
	NSMutableDictionary *tempDict = [_preferences mutableCopy];
	[[tempDict objectForKey: @"PowerDown"] setObject: trackText forKey: @"string"];
	
	_preferences = [[NSDictionary alloc] initWithDictionary: tempDict];
}

@end