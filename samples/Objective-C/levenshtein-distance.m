@interface NSString (levenshteinDistance)
- (NSUInteger)levenshteinDistanceToString:(NSString *)string;
@end

@implementation NSString (levenshteinDistance)
- (NSUInteger)levenshteinDistanceToString:(NSString *)string {
    NSUInteger sl = [self length];
    NSUInteger tl = [string length];
    NSUInteger *d = calloc(sizeof(*d), (sl+1) * (tl+1));

#define d(i, j) d[((j) * sl) + (i)]
    for (NSUInteger i = 0; i <= sl; i++) {
        d(i, 0) = i;
    }
    for (NSUInteger j = 0; j <= tl; j++) {
        d(0, j) = j;
    }
    for (NSUInteger j = 1; j <= tl; j++) {
        for (NSUInteger i = 1; i <= sl; i++) {
            if ([self characterAtIndex:i-1] == [string characterAtIndex:j-1]) {
                d(i, j) = d(i-1, j-1);
            } else {
                d(i, j) = MIN(d(i-1, j), MIN(d(i, j-1), d(i-1, j-1))) + 1;
            }
        }
    }

    NSUInteger r = d(sl, tl);
#undef d

    free(d);

    return r;
}
@end
