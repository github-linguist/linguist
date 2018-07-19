#include <stdio.h>
#include <stdlib.h>
#include <math.h> /* for NaN */

enum fps_type {
        FPS_CONST = 0,
        FPS_ADD,
        FPS_SUB,
        FPS_MUL,
        FPS_DIV,
        FPS_DERIV,
        FPS_INT,
};

typedef struct fps_t *fps;
typedef struct fps_t {
        int type;
        fps s1, s2;
        double a0;
} fps_t;

fps fps_new()
{
        fps x = malloc(sizeof(fps_t));
        x->a0 = 0;
        x->s1 = x->s2 = 0;
        x->type = 0;
        return x;
}

/* language limit of C; when self or mutual recursive definition is needed,
 * one has to be defined, then defined again after it's used.  See how
 * sin and cos are defined this way below
 */
void fps_redefine(fps x, int op, fps y, fps z)
{
        x->type = op;
        x->s1 = y;
        x->s2 = z;
}

fps _binary(fps x, fps y, int op)
{
        fps s = fps_new();
        s->s1 = x;
        s->s2 = y;
        s->type = op;
        return s;
}

fps _unary(fps x, int op)
{
        fps s = fps_new();
        s->s1 = x;
        s->type = op;
        return s;
}

/* Taking the n-th term of series.  This is where actual work is done. */
double term(fps x, int n)
{
        double ret = 0;
        int i;

        switch (x->type) {
        case FPS_CONST: return n > 0 ? 0 : x->a0;
        case FPS_ADD:
                ret = term(x->s1, n) + term(x->s2, n); break;

        case FPS_SUB:
                ret = term(x->s1, n) - term(x->s2, n); break;

        case FPS_MUL:
                for (i = 0; i <= n; i++)
                        ret += term(x->s1, i) * term(x->s2, n - i);
                break;

        case FPS_DIV:
                if (! term(x->s2, 0)) return NAN;

                ret = term(x->s1, n);
                for (i = 1; i <= n; i++)
                        ret -= term(x->s2, i) * term(x, n - i) / term(x->s2, 0);
                break;

        case FPS_DERIV:
                ret = n * term(x->s1, n + 1);
                break;

        case FPS_INT:
                if (!n) return x->a0;
                ret = term(x->s1, n - 1) / n;
                break;

        default:
                fprintf(stderr, "Unknown operator %d\n", x->type);
                exit(1);
        }

        return ret;
}

#define _add(x, y) _binary(x, y, FPS_ADD)
#define _sub(x, y) _binary(x, y, FPS_SUB)
#define _mul(x, y) _binary(x, y, FPS_MUL)
#define _div(x, y) _binary(x, y, FPS_DIV)
#define _integ(x)  _unary(x, FPS_INT)
#define _deriv(x)  _unary(x, FPS_DERIV)

fps fps_const(double a0)
{
        fps x = fps_new();
        x->type = FPS_CONST;
        x->a0 = a0;
        return x;
}

int main()
{
        int i;
        fps one = fps_const(1);
        fps fcos = fps_new();           /* cosine */
        fps fsin = _integ(fcos);        /* sine */
        fps ftan = _div(fsin, fcos);    /* tangent */

        /* redefine cos to complete the mutual recursion; maybe it looks
         * better if I said
         *     *fcos = *( _sub(one, _integ(fsin)) );
         */
        fps_redefine(fcos, FPS_SUB, one, _integ(fsin));

        fps fexp = fps_const(1);        /* exponential */
        /* make exp recurse on self */
        fps_redefine(fexp, FPS_INT, fexp, 0);

        printf("Sin:");   for (i = 0; i < 10; i++) printf(" %g", term(fsin, i));
        printf("\nCos:"); for (i = 0; i < 10; i++) printf(" %g", term(fcos, i));
        printf("\nTan:"); for (i = 0; i < 10; i++) printf(" %g", term(ftan, i));
        printf("\nExp:"); for (i = 0; i < 10; i++) printf(" %g", term(fexp, i));

        return 0;
}
