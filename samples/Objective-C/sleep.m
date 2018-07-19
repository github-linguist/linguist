#import <Foundation/Foundation.h>

int main()
{
  @autoreleasepool {

    NSTimeInterval sleeptime;
    printf("wait time in seconds: ");
    scanf("%f", &sleeptime);

    NSLog(@"sleeping...");
    [NSThread sleepForTimeInterval: sleeptime];
    NSLog(@"awakening...");

  }
  return 0;
}
