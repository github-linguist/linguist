NSFileManager *fm = [NSFileManager defaultManager];
NSLog(@"input.txt %s", [fm fileExistsAtPath:@"input.txt"] ? @"exists" : @"doesn't exist");
NSLog(@"docs %s", [fm fileExistsAtPath:@"docs"] ? @"exists" : @"doesn't exist");
