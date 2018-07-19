NSFileManager *fm = [NSFileManager defaultManager];

[fm createFileAtPath:@"output.txt" contents:[NSData data] attributes:nil];
// Pre-OS X 10.5
[fm createDirectoryAtPath:@"docs" attributes:nil];
// OS X 10.5+
[fm createDirectoryAtPath:@"docs" withIntermediateDirectories:NO attributes:nil error:NULL];
