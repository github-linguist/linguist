typedef enum { kOrdNone, kOrdLex, kOrdByAddress, kOrdNumeric } SortOrder;

@interface MyArray : NSObject {}
// . . .
@end

@implementation MyArray

- (void)sort {
    [self sortWithOrdering:kOrdLex onColumn:0 reversed:NO];
}

- (void)sortWithOrdering:(SortOrder)ord {
    [self sortWithOrdering:ord onColumn:0 reversed:NO];
}

- (void)sortWithOrdering:(SortOrder)ord onColumn:(int)col {
    [self sortWithOrdering:ord onColumn:col reversed:NO];
}

- (void)sortWithOrdering:(SortOrder)ord onColumn:(int)col reversed:(BOOL)rev {
    // . . . Actual sort goes here . . .
}

@end
