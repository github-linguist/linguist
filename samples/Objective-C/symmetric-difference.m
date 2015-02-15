#import <Foundation/Foundation.h>

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSSet* setA = [NSSet setWithObjects:@"John", @"Serena", @"Bob", @"Mary", @"Serena", nil];
    NSSet* setB = [NSSet setWithObjects:@"Jim", @"Mary", @"John", @"Jim", @"Bob", nil];

    // Present our initial data set
    NSLog(@"In set A: %@", setA);
    NSLog(@"In set B: %@", setB);

    // Get our individual differences.
    NSMutableSet* notInSetA = [NSMutableSet setWithSet:setB];
    [notInSetA minusSet:setA];
    NSMutableSet* notInSetB = [NSMutableSet setWithSet:setA];
    [notInSetB minusSet:setB];

    // The symmetric difference is the concatenation of the two individual differences
    NSMutableSet* symmetricDifference = [NSMutableSet setWithSet:notInSetA];
    [symmetricDifference unionSet:notInSetB];

    // Present our results
    NSLog(@"Not in set A: %@", notInSetA);
    NSLog(@"Not in set B: %@", notInSetB);
    NSLog(@"Symmetric Difference: %@", symmetricDifference);

  }
  return 0;
}
