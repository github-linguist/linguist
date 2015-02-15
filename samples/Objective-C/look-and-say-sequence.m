#import <Foundation/Foundation.h>

-(NSString*)lookAndSay:(NSString *)word{
    if (!word) {
        return nil;
    }
    NSMutableString *result = [NSMutableString new];

    char repeat = [word characterAtIndex:0];
    int times = 1;
    word = [NSString stringWithFormat:@"%@ ",[word substringFromIndex:1] ];

    for (NSInteger index = 0; index < word.length; index++) {
        char actual = [word characterAtIndex:index];
        if (actual != repeat) {
            [result appendFormat:@"%d%c", times, repeat];
            times = 1;
            repeat = actual;
        } else {
            times ++;
        }
    }

    return [result copy];
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    NSString *num = @"1";
    for (int i=1;i<=10;i++) {
        NSLog(@"%@", num);

        num = [self lookAndSay:num];
    }
}
