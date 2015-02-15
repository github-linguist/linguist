NSData *data = [NSData dataWithContentsOfFile:@"input.txt"];

[data writeToFile:@"output.txt" atomically:YES];
