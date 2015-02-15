#import <Foundation/Foundation.h>

int main(int argc, const char * argv[])
{
    @autoreleasepool {
        if(argc > 1)
        {
            NSString *arg1 = [NSString stringWithCString:argv[1] encoding:NSUTF8StringEncoding];
            // encoding shouldn't matter in this case
            double kelvin = [arg1 doubleValue];

            NSLog(@"K %.2f",kelvin);
            NSLog(@"C %.2f\n", kelvin - 273.15);
            NSLog(@"F %.2f\n", (kelvin * 1.8) - 459.67);
            NSLog(@"R %.2f", kelvin * 1.8);
        }
    }
    return 0;
}
