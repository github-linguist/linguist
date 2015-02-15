#import <Foundation/Foundation.h>

int main (int argc, const char *argv[]) {
  @autoreleasepool {

    NSSet *s1 = [NSSet setWithObjects:@"a", @"b", @"c", @"d", @"e", nil];
    NSSet *s2 = [NSSet setWithObjects:@"b", @"c", @"d", @"e", @"f", @"h", nil];
    NSSet *s3 = [NSSet setWithObjects:@"b", @"c", @"d", nil];
    NSSet *s4 = [NSSet setWithObjects:@"b", @"c", @"d", nil];
    NSLog(@"s1: %@", s1);
    NSLog(@"s2: %@", s2);
    NSLog(@"s3: %@", s3);
    NSLog(@"s4: %@", s4);

    // Membership
    NSLog(@"b in s1: %d", [s1 containsObject:@"b"]);
    NSLog(@"f in s1: %d", [s1 containsObject:@"f"]);

    // Union
    NSMutableSet *s12 = [NSMutableSet setWithSet:s1];
    [s12 unionSet:s2];
    NSLog(@"s1 union s2: %@", s12);

    // Intersection
    NSMutableSet *s1i2 = [NSMutableSet setWithSet:s1];
    [s1i2 intersectSet:s2];
    NSLog(@"s1 intersect s2: %@", s1i2);

    // Difference
    NSMutableSet *s1_2 = [NSMutableSet setWithSet:s1];
    [s1_2 minusSet:s2];
    NSLog(@"s1 - s2: %@", s1_2);

    // Subset of
    NSLog(@"s3 subset of s1: %d", [s3 isSubsetOfSet:s1]);

    // Equality
    NSLog(@"s3 = s4: %d", [s3 isEqualToSet:s4]);

    // Cardinality
    NSLog(@"size of s1: %lu", [s1 count]);

    // Has intersection (not disjoint)
    NSLog(@"does s1 intersect s2? %d", [s1 intersectsSet:s2]);

    // Adding and removing elements from a mutable set
    NSMutableSet *mut_s1 = [NSMutableSet setWithSet:s1];
    [mut_s1 addObject:@"g"];
    NSLog(@"mut_s1 after adding g: %@", mut_s1);
    [mut_s1 addObject:@"b"];
    NSLog(@"mut_s1 after adding b again: %@", mut_s1);
    [mut_s1 removeObject:@"c"];
    NSLog(@"mut_s1 after removing c: %@", mut_s1);

  }
  return 0;
}
