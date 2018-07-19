NSArray *args = [[NSProcessInfo processInfo] arguments];
NSLog(@"This program is named %@.", [args objectAtIndex:0]);
NSLog(@"There are %d arguments.", [args count] - 1);
for (i = 1; i < [args count]; ++i){
    NSLog(@"the argument #%d is %@", i, [args objectAtIndex:i]);
}
