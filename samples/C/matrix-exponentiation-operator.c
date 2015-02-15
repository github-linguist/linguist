#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct squareMtxStruct {
    int   dim;
    double *cells;
    double **m;
} *SquareMtx;

/* function for initializing row r of a new matrix */
typedef void (*FillFunc)( double *cells, int r, int dim, void *ff_data);

SquareMtx NewSquareMtx( int dim, FillFunc fillFunc, void *ff_data )
{
    SquareMtx sm = malloc(sizeof(struct squareMtxStruct));
    if (sm) {
        int rw;
        sm->dim = dim;
        sm->cells = malloc(dim*dim * sizeof(double));
        sm->m = malloc( dim * sizeof(double *));
        if ((sm->cells != NULL) && (sm->m != NULL)) {
            for (rw=0; rw<dim; rw++) {
                sm->m[rw] = sm->cells + dim*rw;
                fillFunc( sm->m[rw], rw, dim, ff_data );
            }
        }
        else {
            free(sm->m);
            free(sm->cells);
            free(sm);
            printf("Square Matrix allocation failure\n");
            return NULL;
        }
    }
    else {
        printf("Malloc failed for square matrix\n");
    }
    return sm;
}

void ffMatxSquare( double *cells, int rw, int dim, SquareMtx m0 )
{
    int col, ix;
    double sum;
    double *m0rw = m0->m[rw];

    for (col = 0; col < dim; col++) {
        sum = 0.0;
        for (ix=0; ix<dim; ix++)
            sum += m0rw[ix] * m0->m[ix][col];
        cells[col] = sum;
    }
}

void ffMatxMulply( double *cells, int rw, int dim, SquareMtx mplcnds[] )
{
    SquareMtx mleft = mplcnds[0];
    SquareMtx mrigt = mplcnds[1];
    double sum;
    double *m0rw = mleft->m[rw];
    int col, ix;

    for (col = 0; col < dim; col++) {
        sum = 0.0;
        for (ix=0; ix<dim; ix++)
            sum += m0rw[ix] * mrigt->m[ix][col];
        cells[col] = sum;
    }
}

void MatxMul( SquareMtx mr, SquareMtx left, SquareMtx rigt)
{
    int rw;
    SquareMtx mplcnds[2];
    mplcnds[0] = left; mplcnds[1] = rigt;

    for (rw = 0; rw < left->dim; rw++)
        ffMatxMulply( mr->m[rw], rw, left->dim, mplcnds);
}

void ffIdentity( double *cells, int rw, int dim, void *v )
{
    int col;
    for (col=0; col<dim; col++) cells[col] = 0.0;
    cells[rw] = 1.0;
}
void ffCopy(double *cells, int rw, int dim, SquareMtx m1)
{
    int col;
    for (col=0; col<dim; col++) cells[col] = m1->m[rw][col];
}

void FreeSquareMtx( SquareMtx m )
{
    free(m->m);
    free(m->cells);
    free(m);
}

SquareMtx SquareMtxPow( SquareMtx m0, int exp )
{
    SquareMtx v0 = NewSquareMtx(m0->dim, ffIdentity, NULL);
    SquareMtx v1 = NULL;
    SquareMtx base0 = NewSquareMtx( m0->dim, ffCopy, m0);
    SquareMtx base1 = NULL;
    SquareMtx mplcnds[2], t;

    while (exp) {
        if (exp % 2) {
            if (v1)
                MatxMul( v1, v0, base0);
            else  {
                mplcnds[0] = v0; mplcnds[1] = base0;
                v1 = NewSquareMtx(m0->dim, ffMatxMulply, mplcnds);
            }
            {t = v0; v0=v1; v1 = t;}
        }
        if (base1)
            MatxMul( base1, base0, base0);
        else
            base1 = NewSquareMtx( m0->dim, ffMatxSquare, base0);
        t = base0; base0 = base1; base1 = t;
        exp = exp/2;
    }
    if (base0) FreeSquareMtx(base0);
    if (base1) FreeSquareMtx(base1);
    if (v1) FreeSquareMtx(v1);
    return v0;
}

FILE *fout;
void SquareMtxPrint( SquareMtx mtx, const char *mn )
{
    int rw, col;
    int d = mtx->dim;

    fprintf(fout, "%s dim:%d =\n", mn, mtx->dim);

    for (rw=0; rw<d; rw++) {
        fprintf(fout, " |");
        for(col=0; col<d; col++)
            fprintf(fout, "%8.5f ",mtx->m[rw][col] );
        fprintf(fout, " |\n");
    }
    fprintf(fout, "\n");
}

void fillInit( double *cells, int rw, int dim, void *data)
{
    double theta = 3.1415926536/6.0;
    double c1 = cos( theta);
    double s1 = sin( theta);

    switch(rw) {
    case 0:
        cells[0]=c1; cells[1]=s1; cells[2]=0.0;
        break;
    case 1:
        cells[0]=-s1; cells[1]=c1; cells[2]=0;
        break;
    case 2:
        cells[0]=0.0; cells[1]=0.0; cells[2]=1.0;
        break;
    }
}

int main()
{
    SquareMtx m0 = NewSquareMtx( 3, fillInit, NULL);
    SquareMtx m1 = SquareMtxPow( m0, 5);
    SquareMtx m2 = SquareMtxPow( m0, 9);
    SquareMtx m3 = SquareMtxPow( m0, 2);

//  fout = stdout;
    fout = fopen("matrx_exp.txt", "w");
    SquareMtxPrint(m0, "m0"); FreeSquareMtx(m0);
    SquareMtxPrint(m1, "m0^5"); FreeSquareMtx(m1);
    SquareMtxPrint(m2, "m0^9"); FreeSquareMtx(m2);
    SquareMtxPrint(m3, "m0^2"); FreeSquareMtx(m3);
    fclose(fout);

    return 0;
}
