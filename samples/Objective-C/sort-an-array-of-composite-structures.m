@interface Pair : NSObject {
    NSString *name;
    NSString *value;
}
+(instancetype)pairWithName:(NSString *)n value:(NSString *)v;
-(instancetype)initWithName:(NSString *)n value:(NSString *)v;
-(NSString *)name;
-(NSString *)value;
@end

@implementation Pair
+(instancetype)pairWithName:(NSString *)n value:(NSString *)v {
    return [[self alloc] initWithName:n value:v];
}
-(instancetype)initWithName:(NSString *)n value:(NSString *)v {
    if ((self = [super init])) {
        name = n;
        value = v;
    }
    return self;
}
-(NSString *)name { return name; }
-(NSString *)value { return value; }
-(NSString *)description {
    return [NSString stringWithFormat:@"< %@ -> %@ >", name, value];
}
@end

int main() {
    @autoreleasepool {

        NSArray *pairs = @[
                       [Pair pairWithName:@"06-07" value:@"Ducks"],
                       [Pair pairWithName:@"00-01" value:@"Avalanche"],
                       [Pair pairWithName:@"02-03" value:@"Devils"],
                       [Pair pairWithName:@"01-02" value:@"Red Wings"],
                       [Pair pairWithName:@"03-04" value:@"Lightning"],
                       [Pair pairWithName:@"04-05" value:@"lockout"],
                       [Pair pairWithName:@"05-06" value:@"Hurricanes"],
                       [Pair pairWithName:@"99-00" value:@"Devils"],
                       [Pair pairWithName:@"07-08" value:@"Red Wings"],
                       [Pair pairWithName:@"08-09" value:@"Penguins"]];

        // optional 3rd arg: you can also specify a selector to compare the keys
        NSSortDescriptor *sd = [[NSSortDescriptor alloc] initWithKey:@"name" ascending:YES];

        // it takes an array of sort descriptors, and it will be ordered by the
        // first one, then if it's a tie by the second one, etc.
        NSArray *sorted = [pairs sortedArrayUsingDescriptors:@[sd]];
        NSLog(@"%@", sorted);

    }

    return 0;
}
