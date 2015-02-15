#import <Foundation/Foundation.h>

@interface MovingAverage : NSObject {
	unsigned int period;
	NSMutableArray *window;
	double sum;
}
- (instancetype)initWithPeriod:(unsigned int)thePeriod;
@end

@implementation MovingAverage

// init with default period
- (instancetype)init {
	self = [super init];
	if(self) {
		period = 10;
		window = [[NSMutableArray alloc] init];
		sum = 0.0;
	}
	return self;
}

// init with specified period
- (instancetype)initWithPeriod:(unsigned int)thePeriod {
	self = [super init];
	if(self) {
		period = thePeriod;
		window = [[NSMutableArray alloc] init];
		sum = 0.0;
	}
	return self;
}

// add a new number to the window
- (void)add:(double)val {
	sum += val;
	[window addObject:@(val)];
	if([window count] > period) {
		NSNumber *n = window[0];
		sum -= [n doubleValue];
		[window removeObjectAtIndex:0];
	}
}

// get the average value
- (double)avg {
	if([window count] == 0) {
		return 0; // technically the average is undefined
	}
	return sum / [window count];
}

// set the period, resizes current window
- (void)setPeriod:(unsigned int)thePeriod {
	// make smaller?
	if(thePeriod < [window count]) {
		for(int i = 0; i < thePeriod; ++i) {
			NSNumber *n = window[0];
			sum -= [n doubleValue];
			[window removeObjectAtIndex:0];
		}
	}
	period = thePeriod;
}

// get the period (window size)
- (unsigned int)period {
	return period;
}

// clear the window and current sum
- (void)clear {
	[window removeAllObjects];
	sum = 0;
}

@end

int main (int argc, const char * argv[]) {
	@autoreleasepool {
		double testData[10] = {1,2,3,4,5,5,4,3,2,1};
		int periods[2] = {3,5};
		for(int i = 0; i < 2; ++i) {
			MovingAverage *ma = [[MovingAverage alloc] initWithPeriod:periods[i]];
			for(int j = 0; j < 10; ++j) {
				[ma add:testData[j]];
				NSLog(@"Next number = %f, SMA = %f", testData[j], [ma avg]);
			}
			NSLog(@"\n");
		}
	}
	return 0;
}
