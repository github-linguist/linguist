#import <Foundation/Foundation.h>

int main()
{
  @autoreleasepool {

    NSString *s = @"hello";
    printf("%s%s\n", [s UTF8String], " literal");

    NSString *s2 = [s stringByAppendingString:@" literal"];
    // or, NSString *s2 = [NSString stringWithFormat:@"%@%@", s, @" literal"];
    puts([s2 UTF8String]);
    /* or */
    NSMutableString *s3 = [NSMutableString stringWithString: s];
    [s3 appendString: @" literal"];
    puts([s3 UTF8String]);

  }
  return 0;
}
