#import <Foundation/Foundation.h>

// a fantasy two level hierarchy
@interface Animal : NSObject <NSCoding>
{
  NSString *animalName;
  int numberOfLegs;
}
- (instancetype) initWithName: (NSString*)name andLegs: (NSInteger)legs;
- (void) dump;
@end

@implementation Animal
- (instancetype) initWithName: (NSString*)name andLegs: (NSInteger)legs
{
  if ((self = [super init])) {
    animalName = [name retain];
    numberOfLegs = legs;
  }
  return self;
}
- (void) dump
{
  NSLog(@"%@ has %d legs", animalName, numberOfLegs);
}
// ========
- (void) encodeWithCoder: (NSCoder*)coder
{
  [coder encodeObject: animalName forKey: @"Animal.name"];
  [coder encodeInt: numberOfLegs forKey: @"Animal.legs"];
}
- (id) initWithCoder: (NSCoder*)coder
{
  if ((self = [super init])) {
    animalName = [[coder decodeObjectForKey: @"Animal.name"] retain];
    numberOfLegs = [coder decodeIntForKey: @"Animal.legs"];
  }
  return self;
}
@end

@interface Mammal : Animal <NSCoding>
{
  BOOL hasFur;
  NSMutableArray *eatenList;
}
- (instancetype) initWithName: (NSString*)name hasFur: (BOOL)fur;
- (void) addEatenThing: (NSString*)thing;
@end

@implementation Mammal
- (instancetype) init
{
  if ((self = [super init])) {
    hasFur = NO;
    eatenList = [[NSMutableArray alloc] initWithCapacity: 10];
  }
  return self;
}
- (instancetype) initWithName: (NSString*)name hasFur: (BOOL)fur
{
  if ((self = [super initWithName: name andLegs: 4])) {
    hasFur = fur;
    eatenList = [[NSMutableArray alloc] initWithCapacity: 10];
  }
  return self;
}
- (void) addEatenThing: (NSString*)thing
{
  [eatenList addObject: thing];
}
- (void) dump
{
  [super dump];
  NSLog(@"has fur? %@", (hasFur) ? @"yes" : @"no" );
  NSLog(@"it has eaten %d things:", [eatenList count]);
  for ( id element in eatenList )
    NSLog(@"it has eaten a %@", element);
  NSLog(@"end of eaten things list");
}
// ========= de/archiving
- (void) encodeWithCoder: (NSCoder*)coder
{
  [super encodeWithCoder: coder];
  [coder encodeBool: numberOfLegs forKey: @"Mammal.hasFur"];
  [coder encodeObject: eatenList forKey: @"Mammal.eaten"];
}
- (id) initWithCoder: (NSCoder*)coder
{
  if ((self = [super initWithCoder: coder])) {
    hasFur = [coder decodeBoolForKey: @"Mammal.hasFur"];
    eatenList = [coder decodeObjectForKey: @"Mammal.eaten"];
  }
  return self;
}
@end


int main()
{
  @autoreleasepool {

    // let us create a fantasy animal
    Animal *anAnimal = [[Animal alloc]
	         initWithName: @"Eptohippos"
	         andLegs: 7
	        ];
    // for some reason an Eptohippos is not an horse with 7 legs,
    // and it is not a mammal, of course...

    // let us create a fantasy mammal (which is an animal too)
    Mammal *aMammal = [[Mammal alloc]
	        initWithName: @"Mammaluc"
	        hasFur: YES
	       ];
    // let us add some eaten stuff...
    [aMammal addEatenThing: @"lamb"];
    [aMammal addEatenThing: @"table"];
    [aMammal addEatenThing: @"web page"];

    // dump anAnimal
    NSLog(@"----- original Animal -----");
    [anAnimal dump];

    // dump aMammal...
    NSLog(@"----- original Mammal -----");
    [aMammal dump];

    // now let us store the objects...
    NSMutableData *data = [[NSMutableData alloc] init];
    NSKeyedArchiver *arch = [[NSKeyedArchiver alloc]
			      initForWritingWithMutableData: data];
    [arch encodeObject: anAnimal forKey: @"Eptohippos"];
    [arch encodeObject: aMammal forKey: @"Mammaluc"];
    [arch finishEncoding];
    [data writeToFile: @"objects.dat" atomically: YES];

    // now we want to retrieve the saved objects...
    NSData *ldata = [[NSData alloc]
		       initWithContentsOfFile: @"objects.dat"];
    NSKeyedUnarchived *darch = [[NSKeyedUnarchiver alloc]
	                         initForReadingWithData: ldata];
    Animal *archivedAnimal = [darch decodeObjectForKey: @"Eptohippos"];
    Mammal *archivedMammal = [darch decodeObjectForKey: @"Mammaluc"];
    [darch finishDecoding];

    // now let's dump/print the objects...
    NSLog(@"\n");
    NSLog(@"----- the archived Animal -----");
    [archivedAnimal dump];
    NSLog(@"----- the archived Mammal -----");
    [archivedMammal dump];

  }
  return EXIT_SUCCESS;
}
