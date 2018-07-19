#import <Foundation/Foundation.h>

const void *PQRetain(CFAllocatorRef allocator, const void *ptr) {
  return (__bridge_retained const void *)(__bridge id)ptr;
}
void PQRelease(CFAllocatorRef allocator, const void *ptr) {
  (void)(__bridge_transfer id)ptr;
}
CFComparisonResult PQCompare(const void *ptr1, const void *ptr2, void *unused) {
  return [(__bridge id)ptr1 compare:(__bridge id)ptr2];
}

@interface Task : NSObject {
  int priority;
  NSString *name;
}
- (id)initWithPriority:(int)p andName:(NSString *)n;
- (NSComparisonResult)compare:(Task *)other;
@end

@implementation Task
- (id)initWithPriority:(int)p andName:(NSString *)n {
  if ((self = [super init])) {
    priority = p;
    name = [n copy];
  }
  return self;
}
- (NSString *)description {
  return [NSString stringWithFormat:@"%d, %@", priority, name];
}
- (NSComparisonResult)compare:(Task *)other {
  if (priority == other->priority)
    return NSOrderedSame;
  else if (priority < other->priority)
    return NSOrderedAscending;
  else
    return NSOrderedDescending;
}
@end

int main (int argc, const char *argv[]) {
  @autoreleasepool {

    CFBinaryHeapCallBacks callBacks = {0, PQRetain, PQRelease, NULL, PQCompare};
    CFBinaryHeapRef pq = CFBinaryHeapCreate(NULL, 0, &callBacks, NULL);

    CFBinaryHeapAddValue(pq, [[Task alloc] initWithPriority:3 andName:@"Clear drains"]);
    CFBinaryHeapAddValue(pq, [[Task alloc] initWithPriority:4 andName:@"Feed cat"]);
    CFBinaryHeapAddValue(pq, [[Task alloc] initWithPriority:5 andName:@"Make tea"]);
    CFBinaryHeapAddValue(pq, [[Task alloc] initWithPriority:1 andName:@"Solve RC tasks"]);
    CFBinaryHeapAddValue(pq, [[Task alloc] initWithPriority:2 andName:@"Tax return"]);

    while (CFBinaryHeapGetCount(pq) != 0) {
      Task *task = (id)CFBinaryHeapGetMinimum(pq);
      NSLog(@"%@", task);
      CFBinaryHeapRemoveMinimumValue(pq);
    }

    CFRelease(pq);

  }
  return 0;
}
