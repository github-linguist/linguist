%hook ABC
- (id)a:(B)b {
	%log;
	return %orig(nil);
}
%end

%subclass DEF: NSObject
- (id)init {
	[%c(RuntimeAccessibleClass) alloc];
	return nil;
}
%end

%group OptionalHooks
%hook ABC
- (void)release {
	[self retain];
	%orig;
}
%end
%end

%ctor {
	%init;
	if(OptionalCondition)
		%init(OptionalHooks);
}
