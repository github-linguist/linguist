#import <Foundation/Foundation.h>

@interface RCListElement : NSObject
{
  RCListElement *next;
  id datum;
}
- (RCListElement *)next;
- (id)datum;
- (RCListElement *)setNext: (RCListElement *)nx;
- (void)setDatum: (id)d;
@end

@implementation RCListElement
- (RCListElement *)next
{
  return next;
}
- (id)datum
{
  return datum;
}
- (RCListElement *)setNext: (RCListElement *)nx
{
  RCListElement *p;
  p = next;
  next = nx;
  return p;
}
- (void)setDatum: (id)d
{
  datum = d;
}
@end
