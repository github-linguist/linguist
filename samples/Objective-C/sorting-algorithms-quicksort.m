void quicksortInPlace(NSMutableArray *array, NSInteger first, NSInteger last, NSComparator comparator) {
    if (first >= last) return;
    id pivot = array[(first + last) / 2];
    NSInteger left = first;
    NSInteger right = last;
    while (left <= right) {
        while (comparator(array[left], pivot) == NSOrderedAscending)
            left++;
        while (comparator(array[right], pivot) == NSOrderedDescending)
            right--;
        if (left <= right)
            [array exchangeObjectAtIndex:left++ withObjectAtIndex:right--];
    }
    quicksortInPlace(array, first, right, comparator);
    quicksortInPlace(array, left, last, comparator);
}

NSArray* quicksort(NSArray *unsorted, NSComparator comparator) {
    NSMutableArray *a = [NSMutableArray arrayWithArray:unsorted];
    quicksortInPlace(a, 0, a.count - 1, comparator);
    return a;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray *a = @[ @1, @3, @5, @7, @9, @8, @6, @4, @2 ];
        NSLog(@"Unsorted: %@", a);
        NSLog(@"Sorted: %@", quicksort(a, ^(id x, id y) { return [x compare:y]; }));
        NSArray *b = @[ @"Emil", @"Peg", @"Helen", @"Juergen", @"David", @"Rick", @"Barb", @"Mike", @"Tom" ];
        NSLog(@"Unsorted: %@", b);
        NSLog(@"Sorted: %@", quicksort(b, ^(id x, id y) { return [x compare:y]; }));
    }
    return 0;
}
