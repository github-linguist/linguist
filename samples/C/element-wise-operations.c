#include <math.h>

#define for_i for(i = 0; i < h; i++)
#define for_j for(j = 0; j < w; j++)
#define _M double**
#define OPM(name, _op_) \
	void eop_##name(_M a, _M b, _M c, int w, int h){int i,j;\
		for_i for_j c[i][j] = a[i][j] _op_ b[i][j];}
OPM(add, +);OPM(sub, -);OPM(mul, *);OPM(div, /);

#define OPS(name, res) \
	void eop_s_##name(_M a, double s, _M b, int w, int h) {double x;int i,j;\
		for_i for_j {x = a[i][j]; b[i][j] = res;}}
OPS(mul, x*s);OPS(div, x/s);OPS(add, x+s);OPS(sub, x-s);OPS(pow, pow(x, s));
