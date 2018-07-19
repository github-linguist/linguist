NSString *encoded = @"http%3A%2F%2Ffoo%20bar%2F";
NSString *normal = [encoded stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
NSLog(@"%@", normal);
