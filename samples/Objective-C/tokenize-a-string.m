NSString *text = @"Hello,How,Are,You,Today";
NSArray *tokens = [text componentsSeparatedByString:@","];
NSString *result = [tokens componentsJoinedByString:@"."];
NSLog(result);
