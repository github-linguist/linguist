NSArray *haystack = @[@"Zig",@"Zag",@"Wally",@"Ronald",@"Bush",@"Krusty",@"Charlie",@"Bush",@"Bozo"];
for (id needle in @[@"Washington",@"Bush"]) {
    int index = [haystack indexOfObject:needle];
    if (index == NSNotFound)
        NSLog(@"%@ is not in haystack", needle);
    else
        NSLog(@"%i %@", index, needle);
}
