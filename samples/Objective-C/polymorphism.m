#import <Foundation/Foundation.h>

@interface RCPoint : NSObject {
  int x, y;
}
-(instancetype)initWithX:(int)x0;
-(instancetype)initWithX:(int)x0 andY:(int)y0;
-(instancetype)initWithPoint:(RCPoint *)p;
@property (nonatomic) int x;
@property (nonatomic) int y;
@end

@implementation RCPoint
@synthesize x, y;
-(instancetype)initWithX:(int)x0 { return [self initWithX:x0 andY:0]; }
-(instancetype)initWithX:(int)x0 andY:(int)y0 {
  if ((self = [super init])) {
    x = x0;
    y = y0;
  }
  return self;
}
-(instancetype)initWithPoint:(RCPoint *)p { return [self initWithX:p.x andY:p.y]; }
-(NSString *)description { return [NSString stringWithFormat:@"<RCPoint %p x: %d y: %d>", self, x, y]; }
@end

@interface RCCircle : RCPoint {
  int r;
}
-(instancetype)initWithCenter:(RCPoint *)p andRadius:(int)r0;
-(instancetype)initWithX:(int)x0 andY:(int)y0 andRadius:(int)r0;
-(instancetype)initWithCircle:(RCCircle *)c;
@property (nonatomic) int r;
@end

@implementation RCCircle
@synthesize r;
-(instancetype)initWithCenter:(RCPoint *)p andRadius:(int)r0 {
  if ((self = [super initWithPoint:p])) {
    r = r0;
  }
  return self;
}
-(instancetype)initWithX:(int)x0 andY:(int)y0 andRadius:(int)r0 {
  if ((self = [super initWithX:x0 andY:y0])) {
    r = r0;
  }
  return self;
}
-(instancetype)initWithCircle:(RCCircle *)c { return [self initWithX:c.x andY:c.y andRadius:c.r]; }
-(NSString *)description { return [NSString stringWithFormat:@"<RCCircle %p x: %d y: %d r: %d>", self, x, y, r]; }
@end

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSLog(@"%@", [[RCPoint alloc] init]);
    NSLog(@"%@", [[RCPoint alloc] initWithX:3]);
    NSLog(@"%@", [[RCPoint alloc] initWithX:3 andY:4]);
    NSLog(@"%@", [[RCCircle alloc] init]);
    NSLog(@"%@", [[RCCircle alloc] initWithX:3]);
    NSLog(@"%@", [[RCCircle alloc] initWithX:3 andY:4]);
    NSLog(@"%@", [[RCCircle alloc] initWithX:3 andY:4 andRadius:7]);
    RCPoint *p = [[RCPoint alloc] initWithX:1 andY:2];
    NSLog(@"%@", [[RCCircle alloc] initWithPoint:p]);
    NSLog(@"%@", [[RCCircle alloc] initWithCenter:p andRadius:7]);
    NSLog(@"%d", p.x); // 1
    p.x = 8;
    NSLog(@"%d", p.x); // 8

  }
  return 0;
}
