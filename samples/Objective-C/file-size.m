NSFileManager *fm = [NSFileManager defaultManager];

// Pre-OS X 10.5
NSLog(@"%llu", [[fm fileAttributesAtPath:@"input.txt" traverseLink:YES] fileSize]);

// OS X 10.5+
NSLog(@"%llu", [[fm attributesOfItemAtPath:@"input.txt" error:NULL] fileSize]);
