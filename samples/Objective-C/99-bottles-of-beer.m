#import <Foundation/Foundation.h>

int main()
{
    @autoreleasepool {
        int bottles = 99;
        do
        {
            NSLog(@"%i bottles of beer on the wall\n", bottles);
            NSLog(@"%i bottles of beer\n", bottles);
            NSLog(@"Take one down, pass it around\n");
            NSLog(@"%i bottles of beer on the wall\n\n", --bottles);
        } while (bottles > 0);
	
    }
    return 0;
}
