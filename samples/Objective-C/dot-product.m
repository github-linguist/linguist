#import <stdio.h>
#import <stdint.h>
#import <stdlib.h>
#import <string.h>
#import <Foundation/Foundation.h>

// this class exists to return a result between two
// vectors: if vectors have different "size", valid
// must be NO
@interface VResult : NSObject
{
 @private
  double value;
  BOOL valid;
}
+(instancetype)new: (double)v isValid: (BOOL)y;
-(instancetype)init: (double)v isValid: (BOOL)y;
-(BOOL)isValid;
-(double)value;
@end

@implementation VResult
+(instancetype)new: (double)v isValid: (BOOL)y
{
  return [[self alloc] init: v isValid: y];
}
-(instancetype)init: (double)v isValid: (BOOL)y
{
  if ((self == [super init])) {
    value = v;
    valid = y;
  }
  return self;
}
-(BOOL)isValid { return valid; }
-(double)value { return value; }
@end


@interface RCVector : NSObject
{
 @private
  double *vec;
  uint32_t size;
}
+(instancetype)newWithArray: (double *)v ofLength: (uint32_t)l;
-(instancetype)initWithArray: (double *)v ofLength: (uint32_t)l;
-(VResult *)dotProductWith: (RCVector *)v;
-(uint32_t)size;
-(double *)array;
-(void)free;
@end

@implementation RCVector
+(instancetype)newWithArray: (double *)v ofLength: (uint32_t)l
{
  return [[self alloc] initWithArray: v ofLength: l];
}
-(instancetype)initWithArray: (double *)v ofLength: (uint32_t)l
{
  if ((self = [super init])) {
    size = l;
    vec = malloc(sizeof(double) * l);
    if ( vec == NULL )
      return nil;
    memcpy(vec, v, sizeof(double)*l);
  }
  return self;
}
-(void)dealloc
{
  free(vec);
}
-(uint32_t)size { return size; }
-(double *)array { return vec; }
-(VResult *)dotProductWith: (RCVector *)v
{
  double r = 0.0;
  uint32_t i, s;
  double *v1;
  if ( [self size] != [v size] ) return [VResult new: r isValid: NO];
  s = [self size];
  v1 = [v array];
  for(i = 0; i < s; i++) {
    r += vec[i] * v1[i];
  }
  return [VResult new: r isValid: YES];
}
@end

double val1[] = { 1, 3, -5 };
double val2[] = { 4,-2, -1 };

int main()
{
  @autoreleasepool {
    RCVector *v1 = [RCVector newWithArray: val1 ofLength: sizeof(val1)/sizeof(double)];
    RCVector *v2 = [RCVector newWithArray: val2 ofLength: sizeof(val1)/sizeof(double)];
    VResult *r = [v1 dotProductWith: v2];
    if ( [r isValid] ) {
      printf("%lf\n", [r value]);
    } else {
      fprintf(stderr, "length of vectors differ\n");
    }
  }
  return 0;
}
