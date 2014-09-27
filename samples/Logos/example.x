%hook NSObject

- (NSString *)description {
  NSString *ret = %orig;
  return [ret stringByAppendingString:@" (of doom)"];
}

%end

%hook NSNumber

%new + (NSNumber *)getLuckyNumber {
  return [NSNumber numberWithInt:4];
}

%end

%group WrongNumbers

%hook NSNumber

+ (NSNumber *)numberWithInt:(int)value {
  return %orig(value + 1);
}

%end

%end

%ctor {
  %init;

  NSNumber *n1 = objc_msgSend(%c(NSNumber), @selector(getLuckyNumber));
  %init(WrongNumbers);
  NSNumber *n2 = [NSNumber performSelector:@selector(getLuckyNumber)];
  
  assert([n1 intValue] + 1 == [n2 intValue]);
}
