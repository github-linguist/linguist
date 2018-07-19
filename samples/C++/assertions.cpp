#include <cassert> // assert.h also works

int main()
{
  int a;
  // ... input or change a here

  assert(a == 42); // Aborts program if a is not 42, unless the NDEBUG macro was defined
                    // when including <cassert>, in which case it has no effect
}
