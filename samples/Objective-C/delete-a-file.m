NSFileManager *fm = [NSFileManager defaultManager];

// Pre-OS X 10.5
[fm removeFileAtPath:@"input.txt" handler:nil];
[fm removeFileAtPath:@"/input.txt" handler:nil];
[fm removeFileAtPath:@"docs" handler:nil];
[fm removeFileAtPath:@"/docs" handler:nil];

// OS X 10.5+
[fm removeItemAtPath:@"input.txt" error:NULL];
[fm removeItemAtPath:@"/input.txt" error:NULL];
[fm removeItemAtPath:@"docs" error:NULL];
[fm removeItemAtPath:@"/docs" error:NULL];
