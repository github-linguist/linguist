#import <stdio.h>

BOOL iseven(int x)
{
  return (x&1) == 0;
}

@interface EthiopicMult : NSObject
+ (int)mult: (int)plier by: (int)plicand;
+ (int)halve: (int)a;
+ (int)double: (int)a;
@end

@implementation EthiopicMult
+ (int)mult: (int)plier by: (int)plicand
{
  int r = 0;
  while(plier >= 1) {
    if ( !iseven(plier) ) r += plicand;
    plier = [EthiopicMult halve: plier];
    plicand = [EthiopicMult double: plicand];
  }
  return r;
}

+ (int)halve: (int)a
{
  return (a>>1);
}

+ (int)double: (int)a
{
  return (a<<1);
}
@end

int main()
{
  @autoreleasepool {
    printf("%d\n", [EthiopicMult mult: 17 by: 34]);
  }
  return 0;
}
