NSString *jsonString = @"{ \"foo\": 1, \"bar\": [10, \"apples\"] }";
id obj = [NSJSONSerialization
    JSONObjectWithData: [jsonString dataUsingEncoding: NSUTF8StringEncoding]
               options: 0
                 error: NULL];
NSLog(@"%@", obj);

NSDictionary *dict = @{ @"blue": @[@1, @2], @"ocean": @"water"};
NSData *jsonData = [NSJSONSerialization dataWithJSONObject: dict
                                                   options: 0
                                                     error: NULL];
NSString *jsonString2 = [[NSString alloc] initWithData: jsonData
                                              encoding: NSUTF8StringEncoding];
NSLog(@"%@", jsonString2);
