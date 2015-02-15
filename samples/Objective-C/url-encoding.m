NSString *normal = @"http://foo bar/";
NSString *encoded = [normal stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
NSLog(@"%@", encoded);
