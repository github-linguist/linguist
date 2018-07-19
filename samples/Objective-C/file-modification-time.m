NSFileManager *fm = [NSFileManager defaultManager];

// Pre-OS X 10.5
NSLog(@"%@", [[fm fileAttributesAtPath:@"input.txt" traverseLink:YES] fileModificationDate]);
[fm changeFileAttributes:[NSDictionary dictionaryWithObject:[NSDate date] forKey:NSFileModificationDate]
                  atPath:@"input.txt"];

// OS X 10.5+
NSLog(@"%@", [[fm attributesOfItemAtPath:@"input.txt" error:NULL] fileModificationDate]);
[fm setAttributes:[NSDictionary dictionaryWithObject:[NSDate date] forKey:NSFileModificationDate]
     ofItemAtPath:@"input.txt" error:NULL];
