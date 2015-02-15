#import <Foundation/Foundation.h>

int main (int argc, const char *argv[]) {
  @autoreleasepool {

    NSData *data = [NSData dataWithContentsOfFile:@(argv[1])];
    NSString *string = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    NSCountedSet *countedSet = [[NSCountedSet alloc] init];
    NSUInteger len = [string length];
    for (NSUInteger i = 0; i < len; i++) {
      unichar c = [string characterAtIndex:i];
      if ([[NSCharacterSet letterCharacterSet] characterIsMember:c])
        [countedSet addObject:@(c)];
    }
    for (NSNumber *chr in countedSet) {
      NSLog(@"%C => %lu", (unichar)[chr integerValue], [countedSet countForObject:chr]);
    }

  }
  return 0;
}
