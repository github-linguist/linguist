#import <Foundation/Foundation.h>

void show_collection(id coll)
{
  for ( id el in coll )
  {
    if ( [coll isKindOfClass: [NSCountedSet class]] ) {
      NSLog(@"%@ appears %lu times", el, [coll countForObject: el]);
    } else if ( [coll isKindOfClass: [NSDictionary class]] ) {
      NSLog(@"%@ -> %@", el, coll[el]);
    } else {
      NSLog(@"%@", el);
    }
  }
  printf("\n");
}

int main()
{
  @autoreleasepool {

    // create an empty set
    NSMutableSet *set = [[NSMutableSet alloc] init];
    // populate it
    [set addObject: @"one"];
    [set addObject: @10];
    [set addObjectsFromArray: @[@"one", @20, @10, @"two"] ];
    // let's show it
    show_collection(set);

    // create an empty counted set (a bag)
    NSCountedSet *cset = [[NSCountedSet alloc] init];
    // populate it
    [cset addObject: @"one"];
    [cset addObject: @"one"];
    [cset addObject: @"two"];
    // show it
    show_collection(cset);

    // create a dictionary
    NSMutableDictionary *dict = [[NSMutableDictionary alloc] init];
    // populate it
    dict[@"four"] = @4;
    dict[@"eight"] = @8;
    // show it
    show_collection(dict);

  }
  return EXIT_SUCCESS;
}
