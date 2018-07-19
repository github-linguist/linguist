#import <Foundation/Foundation.h>

int main(int argc, const char * argv[])
{

    @autoreleasepool {

        NSLog(@"I'm thinking of a number between 1 - 10. Can you guess what it is?\n");

        int rndNumber = (arc4random() % 10) + 1;

        // Debug (Show rndNumber in console)
        //NSLog(@"Random number is %i", rndNumber);

        int userInput;

        do {

            NSLog(@"Input the number below\n");
            scanf("%i", &userInput);

            if (userInput > 10) {

                NSLog(@"Please enter a number less than 10\n");
            }

            if (userInput > 10 || userInput != rndNumber) {

                NSLog(@"Your guess %i is incorrect, please try again", userInput);

            } else {

                NSLog(@"Your guess %i is correct!", userInput);
            }

        } while (userInput > 10 || userInput != rndNumber);
    }
    return 0;
}
