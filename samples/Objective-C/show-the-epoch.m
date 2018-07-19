#import <Foundation/Foundation.h>

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSDate *t = [NSDate dateWithTimeIntervalSinceReferenceDate:0];
    NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];
    [dateFormatter setTimeZone:[NSTimeZone timeZoneWithName:@"UTC"]];
    [dateFormatter setDateFormat:@"yyyy-MM-dd HH:mm:ss ZZ"];
    NSLog(@"%@", [dateFormatter stringFromDate:t]);

  }
  return 0;
}
