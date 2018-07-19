- (NSArray *) toCharArray {
	
	NSMutableArray *characters = [[NSMutableArray alloc] initWithCapacity:[self length]];
	for (int i=0; i < [self length]; i++) {
		NSString *ichar  = [NSString stringWithFormat:@"%C", [self characterAtIndex:i]];
		[characters addObject:ichar];
	}
	
	return characters;
}

+ (BOOL) luhnCheck:(NSString *)stringToTest {
	
	NSArray *stringAsChars = [stringToTest toCharArray];
	
	BOOL isOdd = YES;
	int oddSum = 0;
	int evenSum = 0;
	
	for (int i = [stringToTest length] - 1; i >= 0; i--) {
		
		int digit = [(NSString *)stringAsChars[i] intValue];
		
		if (isOdd)
			oddSum += digit;
		else
			evenSum += digit/5 + (2*digit) % 10;
			
		isOdd = !isOdd;				
	}
	
	return ((oddSum + evenSum) % 10 == 0);
}

BOOL test0 = [self luhnCheck:@"49927398716"]; //Result = YES
BOOL test1 = [self luhnCheck:@"49927398717"]; //Result = NO
BOOL test2 = [self luhnCheck:@"1234567812345678"]; //Result = NO				
BOOL test3 = [self luhnCheck:@"1234567812345670"]; //Result = YES
