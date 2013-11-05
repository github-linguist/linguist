/* Old-style comment. */

// New-style comment.

typedef float foo_t;

#ifndef ZERO
#define ZERO (0.0)
#endif

#define FOO(x) ((x) + \
  ZERO)

__kernel
void foo(__global const foo_t * x, __local foo_t y, const uint n)
{
  barrier(CLK_LOCAL_MEM_FENCE);

  if (n > 42) {
    *x += y;
  }
}

