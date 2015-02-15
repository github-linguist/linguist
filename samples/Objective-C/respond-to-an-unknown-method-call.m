#include <Foundation/Foundation.h>

// The methods need to be declared somewhere
@interface Dummy : NSObject
- (void)grill;
- (void)ding:(NSString *)s;
@end

@interface Example : NSObject
- (void)foo;
- (void)bar;
@end

@implementation Example
- (void)foo {
  NSLog(@"this is foo");
}

- (void)bar {
  NSLog(@"this is bar");
}

- (void)forwardInvocation:(NSInvocation *)inv {
  NSLog(@"tried to handle unknown method %@", NSStringFromSelector([inv selector]));
  NSUInteger n = [[inv methodSignature] numberOfArguments];
  for (NSUInteger i = 0; i < n-2; i++) { // First two arguments are the object and selector.
    id __unsafe_unretained arg;          // We assume that all arguments are objects.
                                         // getArguments: is type-agnostic and does not perform memory management,
                                         //   therefore we must pass it a pointer to an unretained type
    [inv getArgument:&arg atIndex:i+2];
    NSLog(@"argument #%lu: %@", i, arg);
  }
}

// forwardInvocation: does not work without methodSignatureForSelector:
// The runtime uses the signature returned here to construct the invocation.
- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector {
  int numArgs = [[NSStringFromSelector(aSelector) componentsSeparatedByString:@":"] count] - 1;
  // we assume that all arguments are objects
  // The type encoding is "v@:@@@...", where "v" is the return type, void
  // "@" is the receiver, object type; ":" is the selector of the current method;
  // and each "@" after corresponds to an object argument
  return [NSMethodSignature signatureWithObjCTypes:
          [[@"v@:" stringByPaddingToLength:numArgs+3 withString:@"@" startingAtIndex:0] UTF8String]];
}
@end

int main()
{
  @autoreleasepool {

    id example = [[Example alloc] init];

    [example foo];          // prints "this is foo"
    [example bar];          // prints "this is bar"
    [example grill];        // prints "tried to handle unknown method grill"
    [example ding:@"dong"]; // prints "tried to handle unknown method ding:"
                            // prints "argument #0: dong"

  }
  return 0;
}
