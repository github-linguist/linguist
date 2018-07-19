#import <Foundation/Foundation.h>

typedef NSArray *(^SOfN)(id);

SOfN s_of_n_creator(int n) {
  NSMutableArray *sample = [[NSMutableArray alloc] initWithCapacity:n];
  __block int i = 0;
  return [^(id item) {
    i++;
    if (i <= n) {
      [sample addObject:item];
    } else if (rand() % i < n) {
      sample[rand() % n] = item;
    }
    return sample;
  } copy];
}

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSCountedSet *bin = [[NSCountedSet alloc] init];
    for (int trial = 0; trial < 100000; trial++) {
      SOfN s_of_n = s_of_n_creator(3);
      NSArray *sample;
      for (int i = 0; i < 10; i++)
        sample = s_of_n(@(i));
      [bin addObjectsFromArray:sample];
    }
    NSLog(@"%@", bin);

  }
  return 0;
}
