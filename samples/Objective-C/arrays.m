// NSArrays are ordered collections of NSObject subclasses only.

// Create an array of NSString objects.
NSArray *firstArray = [[NSArray alloc] initWithObjects:@"Hewey", @"Louie", @"Dewey", nil];

// NSArrays are immutable; it does have a mutable subclass, however - NSMutableArray.
// Let's instantiate one with a mutable copy of our array.
// We can do this by sending our first array a -mutableCopy message.
NSMutableArray *secondArray = [firstArray mutableCopy];

// Replace Louie with Launchpad McQuack.
[secondArray replaceObjectAtIndex:1 withObject:@"Launchpad"];

// Display the first object in the array.
NSLog(@"%@", [secondArray objectAtIndex:0]);

// In non-ARC or non-GC environments, retained objects must be released later.
[firstArray release];
[secondArray release];

// There is also a modern syntax which allows convenient creation of autoreleased immutable arrays.
// No nil termination is then needed.
NSArray *thirdArray = @[ @"Hewey", @"Louie", @"Dewey", @1, @2, @3 ];
