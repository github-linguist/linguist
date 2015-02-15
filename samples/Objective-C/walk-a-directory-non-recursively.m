NSString *dir = @"/foo/bar";

// Pre-OS X 10.5
NSArray *contents = [[NSFileManager defaultManager] directoryContentsAtPath:dir];
// OS X 10.5+
NSArray *contents = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:dir error:NULL];

for (NSString *file in contents)
  if ([[file pathExtension] isEqualToString:@"mp3"])
    NSLog(@"%@", file);
