#define RMAX32 ((1U << 31) - 1)

//--------------------------------------------------------------------

@interface Rand : NSObject
-(instancetype) initWithSeed: (int)seed;
-(int) next;
@property (nonatomic) long seed;
@end

@implementation Rand
-(instancetype) initWithSeed: (int)seed {
    if ((self = [super init])) {
        self.seed = seed;
    }
    return self;
}
-(int) next {
    return (int) ((_seed = (_seed * 214013 + 2531011) & RMAX32) >> 16);
}
@end

//--------------------------------------------------------------------

@interface Card : NSObject
-(instancetype) initWithSequence: (int)n;
-(instancetype) initWithValue: (int)v suit: (int)s;
@property (nonatomic) int value;
@property (nonatomic) int suit;
@end

@implementation Card
-(instancetype) initWithSequence: (int)n {
    return [self initWithValue:n/4 suit:n%4];
}
-(instancetype) initWithValue: (int)v suit: (int)s {
    if ((self = [super init])) {
        _value = v;  _suit = s;
    }
    return self;
}
-(NSString *) description {
    static NSString * const kSuits = @"♣♦♥♠";
    static NSString * const kValues = @"A23456789TJQK";
    return [NSString stringWithFormat:@"%C%C",
            [kValues characterAtIndex:_value],
            [kSuits characterAtIndex:_suit]];
}
@end

//--------------------------------------------------------------------

@interface Deck : NSObject
-(instancetype) initWithSeed: (int)seed;
@property (nonatomic, strong) NSMutableArray *cards;
@end

@implementation Deck
-(instancetype) initWithSeed: (int)seed {
    if ((self = [super init])) {
        Rand *r = [[Rand alloc] initWithSeed:seed];
        _cards = [NSMutableArray array];
        for (int i = 0; i < 52; i++)
            [_cards addObject:[[Card alloc] initWithSequence:51 - i]];
        for (int i = 0; i < 51; i++)
            [_cards exchangeObjectAtIndex:i withObjectAtIndex:51 - [r next] % (52 - i)];
    }
    return self;
}
-(NSString *) description {
    NSMutableString *s = [NSMutableString string];
    for (int i = 0; i < [_cards count]; i++) {
        [s appendString:[_cards[i] description]];
        [s appendString:i%8==7 ? @"\n" : @" "];
    }
    return s;
}
@end

//--------------------------------------------------------------------

int main(int argc, const char * argv[])
{
    @autoreleasepool {
        NSLog(@"Deck 1\n%@\n", [[Deck alloc] initWithSeed:1]);
        NSLog(@"Deck 617\n%@\n", [[Deck alloc] initWithSeed:617]);
    }
    return 0;
}
