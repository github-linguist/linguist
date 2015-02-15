#import <Foundation/Foundation.h>

int main()
{
  @autoreleasepool {
    int num1, num2;
    scanf("%d %d", &num1, &num2);

    NSLog(@"%d %d", num1, num2);

    NSMutableArray *arr = [NSMutableArray arrayWithCapacity: (num1*num2)];
    // initialize it with 0s
    for(int i=0; i < (num1*num2); i++) [arr addObject: @0];

    // replace 0s with something more interesting
    for(int i=0; i < num1; i++) {
      for(int j=0; j < num2; j++) {
        arr[i*num2+j] = @(i*j);
      }
    }

    // access a value: i*num2+j, where i,j are the indexes for the bidimensional array
    NSLog(@"%@", arr[1*num2+3]);
  }
  return 0;
}
