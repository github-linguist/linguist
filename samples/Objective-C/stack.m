NSMutableArray *stack = [NSMutableArray array]; // creating

[stack addObject:value]; // pushing

id value = [stack lastObject];
[stack removeLastObject]; // popping

[stack count] == 0 // is empty?
