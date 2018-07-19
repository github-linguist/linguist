#import <Foundation/Foundation.h>

@interface NSArray (FlattenExt)
@property (nonatomic, readonly) NSArray *flattened;
@end

@implementation NSArray (FlattenExt)
-(NSArray *) flattened {
    NSMutableArray *flattened = [[NSMutableArray alloc] initWithCapacity:self.count];

    for (id object in self) {
        if ([object isKindOfClass:[NSArray class]])
            [flattened addObjectsFromArray:((NSArray *)object).flattened];
        else
            [flattened addObject:object];
    }

    return [flattened autorelease];
}
@end

int main() {
    @autoreleasepool {
        NSArray *p = @[
		         @[ @1 ],
		         @2,
		         @[ @[@3, @4], @5],
		         @[ @[ @[ ] ] ],
		         @[ @[ @[ @6 ] ] ],
		         @7,
		         @8,
		         @[ ] ];

        for (id object in unflattened.flattened)
            NSLog(@"%@", object);

    }

    return 0;
}
