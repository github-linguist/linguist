#import <Foundation/Foundation.h>

@interface NSString (RCExt)
-(NSString *) ltrim;
-(NSString *) rtrim;
-(NSString *) trim;
@end

@implementation NSString (RCExt)
-(NSString *) ltrim
{
  NSInteger i;
  NSCharacterSet *cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
  for(i = 0; i < [self length]; i++)
  {
    if ( ![cs characterIsMember: [self characterAtIndex: i]] ) break;
  }
  return [self substringFromIndex: i];
}

-(NSString *) rtrim
{
  NSInteger i;
  NSCharacterSet *cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
  for(i = [self length] -1; i >= 0; i--)
  {
    if ( ![cs characterIsMember: [self characterAtIndex: i]] ) break;
  }
  return [self substringToIndex: (i+1)];
}

-(NSString *) trim
{
  return [self
	   stringByTrimmingCharactersInSet:
	     [NSCharacterSet whitespaceAndNewlineCharacterSet]];
}
@end

int main()
{
  @autoreleasepool {

    NSString *s = @"     this is a string     ";

    NSLog(@"'%@'", s);
    NSLog(@"'%@'", [s ltrim]);
    NSLog(@"'%@'", [s rtrim]);
    NSLog(@"'%@'", [s trim]);

  }
  return 0;
}
