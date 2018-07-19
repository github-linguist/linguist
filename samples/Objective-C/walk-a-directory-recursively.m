NSString *dir = NSHomeDirectory();
NSDirectoryEnumerator *de = [[NSFileManager defaultManager] enumeratorAtPath:dir];

for (NSString *file in de)
  if ([[file pathExtension] isEqualToString:@"mp3"])
    NSLog(@"%@", file);
