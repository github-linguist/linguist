@interface U0 : NSObject { }
@end
@interface U1 : NSObject { }
@end
@implementation U0
@end
@implementation U1
@end

void foo();
void bar(int i);
void baz(int i);

void foo() {
  for (int i = 0; i <= 1; i++) {
    @try {
      bar(i);
    } @catch (U0 *e) {
      NSLog(@"Function foo caught exception U0");
    }
  }
}

void bar(int i) {
  baz(i); // Nest those calls
}

void baz(int i) {
  if (i == 0)
    @throw [U0 new];
  else
    @throw [U1 new];
}


int main (int argc, const char * argv[]) {
  @autoreleasepool {

    foo();

  }
  return 0;
}
