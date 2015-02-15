#include <stdio.h>

// 1. Named parameters

typedef struct { int x, y, z; } FTest_args;

void FTest (FTest_args args) {
	printf("x: %d, y: %d, z: %d\n", args.x, args.y, args.z);
}

#define FT(...) FTest((FTest_args){ __VA_ARGS__ })


// 2. Default parameters

#define DFT(...) FTest((FTest_args){ .x = 142, .y = 143, .z = 144, __VA_ARGS__ })


// 3. Convenience wrapper to avoid accessing args as "args.name"

void FTest2 (int x, int y, int z) {
	printf("x: %d, y: %d, z: %d\n", x, y, z);
}

static inline void FTest2_default_wrapper (FTest_args args) {
	return FTest2(args.x, args.y, args.z);
}

#define DF2(...) FTest2_default_wrapper((FTest_args){ .x = 142, .y = 143, .z = 144, __VA_ARGS__ })


int main(int argc, char **argv)
{
	// Named parameters
	FTest((FTest_args){ .y = 10 });
	FTest((FTest_args){ .y = 10, .z = 42 });
	FT( .z = 47, .y = 10, .x = 42 );
	
	// Default parameters
	DFT();
	DFT( .z = 99 );
	
	// Default parameters with wrapper
	DF2();
	DF2( .z = 99 );
	
	return 0;
}
