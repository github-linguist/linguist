#import <Foundation/Foundation.h>

+ (NSArray *)powerSetForArray:(NSArray *)array {
	UInt32 subsetCount = 1 << array.count;
	NSMutableArray *subsets = [NSMutableArray arrayWithCapacity:subsetCount];
	for(int subsetIndex = 0; subsetIndex < subsetCount; subsetIndex++) {
		NSMutableArray *subset = [[NSMutableArray alloc] init];
		for (int itemIndex = 0; itemIndex < array.count; itemIndex++) {
			if((subsetIndex >> itemIndex) & 0x1) {
				[subset addObject:array[itemIndex]];
			}
		}		
		[subsets addObject:subset];
	}
	return subsets;
}
