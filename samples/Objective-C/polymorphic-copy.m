@interface T : NSObject
- (void)identify;
@end

@implementation T
- (void)identify {
    NSLog(@"I am a genuine T");
}
- (id)copyWithZone:(NSZone *)zone {
    T *copy = [[[self class] allocWithZone:zone] init]; // call an appropriate constructor here
                                                        // then copy data into it as appropriate here
                                                        // make sure to use "[[self class] alloc..." and
                                                        // not "[T alloc..." to make it polymorphic
    return copy;
}
@end

@interface S : T
@end

@implementation S
- (void)identify
{
    NSLog(@"I am an S");
}
@end

int main()
{
    @autoreleasepool {

        T *original = [[S alloc] init];
        T *another = [original copy];
        [another identify]; // logs "I am an S"

    }
    return 0;
}
