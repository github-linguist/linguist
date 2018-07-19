NSFileManager *fm = [NSFileManager defaultManager];

// Pre-OS X 10.5
[fm movePath:@"input.txt" toPath:@"output.txt" handler:nil];
[fm movePath:@"docs" toPath:@"mydocs" handler:nil];

// OS X 10.5+
[fm moveItemAtPath:@"input.txt" toPath:@"output.txt" error:NULL];
[fm moveItemAtPath:@"docs" toPath:@"mydocs" error:NULL];
