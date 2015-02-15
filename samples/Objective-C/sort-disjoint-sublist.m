#import <Foundation/Foundation.h>

@interface DisjointSublistView : NSMutableArray {
  NSMutableArray *array;
  int *indexes;
  int num_indexes;
}
- (instancetype)initWithArray:(NSMutableArray *)a andIndexes:(NSIndexSet *)ind;
@end

@implementation DisjointSublistView
- (instancetype)initWithArray:(NSMutableArray *)a andIndexes:(NSIndexSet *)ind {
  if ((self = [super init])) {
    array = a;
    num_indexes = [ind count];
    indexes = malloc(num_indexes * sizeof(int));
    for (NSUInteger i = [ind firstIndex], j = 0; i != NSNotFound; i = [ind indexGreaterThanIndex:i], j++)
      indexes[j] = i;
  }
  return self;
}
- (void)dealloc {
  free(indexes);
}
- (NSUInteger)count { return num_indexes; }
- (id)objectAtIndex:(NSUInteger)i { return array[indexes[i]]; }
- (void)replaceObjectAtIndex:(NSUInteger)i withObject:(id)x { array[indexes[i]] = x; }
@end

@interface NSMutableArray (SortDisjoint)
- (void)sortDisjointSublist:(NSIndexSet *)indexes usingSelector:(SEL)comparator;
@end
@implementation NSMutableArray (SortDisjoint)
- (void)sortDisjointSublist:(NSIndexSet *)indexes usingSelector:(SEL)comparator {
  DisjointSublistView *d = [[DisjointSublistView alloc] initWithArray:self andIndexes:indexes];
  [d sortUsingSelector:comparator];
}
@end

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSMutableArray *a = [@[@7, @6, @5, @4, @3, @2, @1, @0] mutableCopy];
    NSMutableIndexSet *ind = [NSMutableIndexSet indexSet];
    [ind addIndex:6]; [ind addIndex:1]; [ind addIndex:7];
    [a sortDisjointSublist:ind usingSelector:@selector(compare:)];
    NSLog(@"%@", a);

  }
  return 0;
}
