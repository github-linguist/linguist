#import <Foundation/Foundation.h>

@interface GuessNumberFakeArray : NSArray {
  int lower, upper;
}
- (instancetype)initWithLower:(int)l andUpper:(int)u;
@end

@implementation GuessNumberFakeArray
- (instancetype)initWithLower:(int)l andUpper:(int)u {
  if ((self = [super init])) {
    lower = l;
    upper = u;
  }
  return self;
}
- (NSUInteger)count { return upper-lower; }
- (id)objectAtIndex:(NSUInteger)i {
  printf("My guess is: %d. Is it too high, too low, or correct? (H/L/C) ", lower + (int)i);
  char input[2] = " ";
  scanf("%1s", input);
  switch (tolower(input[0])) {
    case 'l':
      return @-1;
    case 'h':
      return @1;
    case 'c':
      return @0;
  }
  return nil;
}
@end

#define LOWER 0
#define UPPER 100

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    printf("Instructions:\n"
           "Think of integer number from %d (inclusive) to %d (exclusive) and\n"
           "I will guess it. After each guess, you respond with L, H, or C depending\n"
           "on if my guess was too low, too high, or correct.\n",
           LOWER, UPPER);
    NSUInteger result = [[[GuessNumberFakeArray alloc] initWithLower:LOWER andUpper:UPPER]
                         indexOfObject:[NSNumber numberWithInt: 0]
                         inSortedRange:NSMakeRange(0, UPPER - LOWER)
                               options:0
                       usingComparator:^(id x, id y){ return [x compare: y]; }];
    if (result == NSNotFound)
      printf("That is impossible.\n");
    else
      printf("Your number is %d.", LOWER + (int)result);

  }
  return 0;
}
