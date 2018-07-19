#include <stdio.h>

typedef char pin_t;
#define IN const pin_t *
#define OUT pin_t *
#define PIN(X) pin_t _##X; pin_t *X = & _##X;
#define V(X) (*(X))

/* a NOT that does not soil the rest of the host of the single bit */
#define NOT(X) (~(X)&1)

/* a shortcut to "implement" a XOR using only NOT, AND and OR gates, as
   task requirements constrain */
#define XOR(X,Y) ((NOT(X)&(Y)) | ((X)&NOT(Y)))

void halfadder(IN a, IN b, OUT s, OUT c)
{
  V(s) = XOR(V(a), V(b));
  V(c) = V(a) & V(b);
}

void fulladder(IN a, IN b, IN ic, OUT s, OUT oc)
{
  PIN(ps); PIN(pc); PIN(tc);

  halfadder(/*INPUT*/a, b, /*OUTPUT*/ps, pc);
  halfadder(/*INPUT*/ps, ic, /*OUTPUT*/s, tc);
  V(oc) = V(tc) | V(pc);
}

void fourbitsadder(IN a0, IN a1, IN a2, IN a3,
		   IN b0, IN b1, IN b2, IN b3,
		   OUT o0, OUT o1, OUT o2, OUT o3,
		   OUT overflow)
{
  PIN(zero); V(zero) = 0;
  PIN(tc0); PIN(tc1); PIN(tc2);

  fulladder(/*INPUT*/a0, b0, zero, /*OUTPUT*/o0, tc0);
  fulladder(/*INPUT*/a1, b1, tc0,  /*OUTPUT*/o1, tc1);
  fulladder(/*INPUT*/a2, b2, tc1,  /*OUTPUT*/o2, tc2);
  fulladder(/*INPUT*/a3, b3, tc2,  /*OUTPUT*/o3, overflow);
}


int main()
{
  PIN(a0); PIN(a1); PIN(a2); PIN(a3);
  PIN(b0); PIN(b1); PIN(b2); PIN(b3);
  PIN(s0); PIN(s1); PIN(s2); PIN(s3);
  PIN(overflow);

  V(a3) = 0; V(b3) = 1;
  V(a2) = 0; V(b2) = 1;
  V(a1) = 1; V(b1) = 1;
  V(a0) = 0; V(b0) = 0;

  fourbitsadder(a0, a1, a2, a3, /* INPUT */
		b0, b1, b2, b3,
		s0, s1, s2, s3, /* OUTPUT */
		overflow);

  printf("%d%d%d%d + %d%d%d%d = %d%d%d%d, overflow = %d\n",
	 V(a3), V(a2), V(a1), V(a0),
	 V(b3), V(b2), V(b1), V(b0),
	 V(s3), V(s2), V(s1), V(s0),
	 V(overflow));

  return 0;
}
