[@"abcd" hasPrefix:@"ab"] //returns true
[@"abcd" hasSuffix:@"zn"] //returns false
int loc = [@"abab" rangeOfString:@"bb"].location //returns -1
loc = [@"abab" rangeOfString:@"ab"].location //returns 0
loc = [@"abab" rangeOfString:@"ab" options:0 range:NSMakeRange(loc+1, [@"abab" length]-(loc+1))].location //returns 2
