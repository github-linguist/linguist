// declare the class to conform to NSStreamDelegate protocol

// in some method
NSOutputStream *oStream;
[NSStream getStreamsToHost:[NSHost hostWithName:@"localhost"] port:256 inputStream:NULL outputStream:&oStream];
[oStream setDelegate:self];
[oStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
[oStream open];


// later, in the same class:
- (void)stream:(NSStream *)aStream handleEvent:(NSStreamEvent)streamEvent {
    NSOutputStream *oStream = (NSOutputStream *)aStream;
    if (streamEvent == NSStreamEventHasBytesAvailable) {
        NSString *str = @"hello socket world";
        const char *rawstring = [str UTF8String];
        [oStream write:rawstring maxLength:strlen(rawstring)];
        [oStream close];
    }
}
