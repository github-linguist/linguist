//
//  TPKA.m
//  RosettaCode
//
//  Created by Alexander Alvonellos on 5/26/12.
//  Trabb Pardo-Knuth algorithm
//

#import <Foundation/Foundation.h>
double f(double x);

double f(double x) {
    return pow(abs(x), 0.5) + 5*(pow(x, 3));
}

int main (int argc, const char * argv[])
{
    @autoreleasepool {
        NSMutableArray *input = [[NSMutableArray alloc] initWithCapacity:0];

        printf("%s", "Instructions: please enter 11 numbers.\n");
        for(int i = 0; i < 11; i++) {
            double userInput = 0.0;
            printf("%s", "Please enter a number: ");
            scanf("%lf", &userInput);
            [input addObject: @(userInput)];
        }

        for(int i = 10; i >= 0; i--) {
            double x = [input[i] doubleValue];
            double y = f(x);
            printf("f(%.2f) \t=\t", x);
            if(y < 400.0) {
                printf("%.2f\n", y);
            } else {
                printf("%s\n", "TOO LARGE");
            }
        }
    }
    return 0;
}
