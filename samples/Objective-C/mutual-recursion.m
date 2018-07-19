#import <Foundation/Foundation.h>

@interface Hofstadter : NSObject
+ (int)M: (int)n;
+ (int)F: (int)n;
@end

@implementation Hofstadter
+ (int)M: (int)n
{
  if ( n == 0 ) return 0;
  return n - [self F: [self M: (n-1)]];
}
+ (int)F: (int)n
{
  if ( n == 0 ) return 1;
  return n - [self M: [self F: (n-1)]];
}
@end

int main()
{
  int i;

  for(i=0; i < 20; i++) {
    printf("%3d ", [Hofstadter F: i]);
  }
  printf("\n");
  for(i=0; i < 20; i++) {
    printf("%3d ", [Hofstadter M: i]);
  }
  printf("\n");
  return 0;
}
