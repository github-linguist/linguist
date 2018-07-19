//--------------------------------------------------------------------
// Person class

@interface Person : NSObject {
    NSUInteger _candidateIndex;
}
@property (nonatomic, strong)   NSString*   name;
@property (nonatomic, strong)   NSArray*    prefs;
@property (nonatomic, weak)     Person*     fiance;
@end

@implementation Person
+ (Person *)named:(NSString *)name {
    return [[Person alloc] initWithName:name];
}
- (id)initWithName:(NSString *)name {
    if ((self = [super init])) {
        _name = name;
        _prefs = nil;
        _fiance = nil;
        _candidateIndex = 0;
    }
    return self;
}
- (BOOL)prefers:(Person *)p {
    return [_prefs indexOfObject:p] < [_prefs indexOfObject:_fiance];
}
- (Person *)nextCandidateNotYetProposedTo {
    if (_candidateIndex >= _prefs.count) return nil;
    return _prefs[_candidateIndex++];
}
- (void)engageTo:(Person *)p {
    if (p.fiance) p.fiance.fiance = nil;
    p.fiance = self;
    if (self.fiance) self.fiance.fiance = nil;
    self.fiance = p;
}
- (NSString *)description {
    return _name;
}
@end

//--------------------------------------------------------------------

BOOL isStable(NSArray *men) {
    NSArray *women = ((Person *)men[0]).prefs;
    for (Person *guy in men) {
        for (Person *gal in women) {
            if ([guy prefers:gal] && [gal prefers:guy])
                return NO;
        }
    }
    return YES;
}

//--------------------------------------------------------------------

void doMarriage() {
    Person *abe  = [Person named:@"abe"];
    Person *bob  = [Person named:@"bob"];
    Person *col  = [Person named:@"col"];
    Person *dan  = [Person named:@"dan"];
    Person *ed   = [Person named:@"ed"];
    Person *fred = [Person named:@"fred"];
    Person *gav  = [Person named:@"gav"];
    Person *hal  = [Person named:@"hal"];
    Person *ian  = [Person named:@"ian"];
    Person *jon  = [Person named:@"jon"];
    Person *abi  = [Person named:@"abi"];
    Person *bea  = [Person named:@"bea"];
    Person *cath = [Person named:@"cath"];
    Person *dee  = [Person named:@"dee"];
    Person *eve  = [Person named:@"eve"];
    Person *fay  = [Person named:@"fay"];
    Person *gay  = [Person named:@"gay"];
    Person *hope = [Person named:@"hope"];
    Person *ivy  = [Person named:@"ivy"];
    Person *jan  = [Person named:@"jan"];

    abe.prefs  = @[ abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay ];
    bob.prefs  = @[ cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay ];
    col.prefs  = @[ hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan ];
    dan.prefs  = @[ ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi ];
    ed.prefs   = @[ jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay ];
    fred.prefs = @[ bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay ];
    gav.prefs  = @[ gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay ];
    hal.prefs  = @[ abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee ];
    ian.prefs  = @[ hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve ];
    jon.prefs  = @[ abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope ];
    abi.prefs  = @[ bob, fred, jon, gav, ian, abe, dan, ed, col, hal ];
    bea.prefs  = @[ bob, abe, col, fred, gav, dan, ian, ed, jon, hal ];
    cath.prefs = @[ fred, bob, ed, gav, hal, col, ian, abe, dan, jon ];
    dee.prefs  = @[ fred, jon, col, abe, ian, hal, gav, dan, bob, ed ];
    eve.prefs  = @[ jon, hal, fred, dan, abe, gav, col, ed, ian, bob ];
    fay.prefs  = @[ bob, abe, ed, ian, jon, dan, fred, gav, col, hal ];
    gay.prefs  = @[ jon, gav, hal, fred, bob, abe, col, ed, dan, ian ];
    hope.prefs = @[ gav, jon, bob, abe, ian, dan, hal, ed, col, fred ];
    ivy.prefs  = @[ ian, col, hal, gav, fred, bob, abe, ed, jon, dan ];
    jan.prefs  = @[ ed, hal, gav, abe, bob, jon, col, ian, fred, dan ];

    NSArray *men = [NSArray arrayWithArray:abi.prefs];

    NSUInteger freeMenCount = men.count;
    while (freeMenCount > 0) {
        for (Person *guy in men) {
            if (guy.fiance == nil) {
                Person *gal = [guy nextCandidateNotYetProposedTo];
                if (gal.fiance == nil) {
                    [guy engageTo:gal];
                    freeMenCount--;
                } else if ([gal prefers:guy]) {
                    [guy engageTo:gal];
                }
            }
        }
    }

    for (Person *guy in men) {
        printf("%s is engaged to %s\n", [guy.name UTF8String], [guy.fiance.name UTF8String]);
    }
    printf("Stable = %d\n", (int)isStable(men));

    printf("\nSwitching fred & jon's partners\n");
    [fred engageTo:abi];
    [jon engageTo:bea];
    printf("Stable = %d\n", (int)isStable(men));
}

//--------------------------------------------------------------------

int main(int argc, const char * argv[])
{
    @autoreleasepool {
        doMarriage();
    }
    return 0;
}
