#import <Foundation/Foundation.h>

@interface Example : NSObject
- (NSNumber *)foo;
@end

@implementation Example
- (NSNumber *)foo {
  return @42;
}
@end

int main (int argc, const char *argv[]) {
  @autoreleasepool {

    id example = [[Example alloc] init];
    SEL selector = @selector(foo); // or = NSSelectorFromString(@"foo");
    NSLog(@"%@", [example performSelector:selector]);

  }
  return 0;
}
