#include <stdio.h>
#define TALLOC(n,typ) malloc(n*sizeof(typ))

#define EL_Type int

typedef struct sMtx {
    int     dim_x, dim_y;
    EL_Type *m_stor;
    EL_Type **mtx;
} *Matrix, sMatrix;

typedef struct sRvec {
    int     dim_x;
    EL_Type *m_stor;
} *RowVec, sRowVec;

Matrix NewMatrix( int x_dim, int y_dim )
{
    int n;
    Matrix m;
    m = TALLOC( 1, sMatrix);
    n = x_dim * y_dim;
    m->dim_x = x_dim;
    m->dim_y = y_dim;
    m->m_stor = TALLOC(n, EL_Type);
    m->mtx = TALLOC(m->dim_y, EL_Type *);
    for(n=0; n<y_dim; n++) {
        m->mtx[n] = m->m_stor+n*x_dim;
    }
    return m;
}

void MtxSetRow(Matrix m, int irow, EL_Type *v)
{
    int ix;
    EL_Type *mr;
    mr = m->mtx[irow];
    for(ix=0; ix<m->dim_x; ix++)
        mr[ix] = v[ix];
}

Matrix InitMatrix( int x_dim, int y_dim, EL_Type **v)
{
    Matrix m;
    int iy;
    m = NewMatrix(x_dim, y_dim);
    for (iy=0; iy<y_dim; iy++)
        MtxSetRow(m, iy, v[iy]);
    return m;
}

void MtxDisplay( Matrix m )
{
    int iy, ix;
    const char *sc;
    for (iy=0; iy<m->dim_y; iy++) {
        printf("   ");
        sc = " ";
        for (ix=0; ix<m->dim_x; ix++) {
            printf("%s %3d", sc, m->mtx[iy][ix]);
            sc = ",";
        }
        printf("\n");
    }
    printf("\n");
}

void MtxMulAndAddRows(Matrix m, int ixrdest, int ixrsrc, EL_Type mplr)
{
    int ix;
    EL_Type *drow, *srow;
    drow = m->mtx[ixrdest];
    srow = m->mtx[ixrsrc];
    for (ix=0; ix<m->dim_x; ix++)
        drow[ix] += mplr * srow[ix];
//	printf("Mul row %d by %d and add to row %d\n", ixrsrc, mplr, ixrdest);
//	MtxDisplay(m);
}

void MtxSwapRows( Matrix m, int rix1, int rix2)
{
    EL_Type *r1, *r2, temp;
    int ix;
    if (rix1 == rix2) return;
    r1 = m->mtx[rix1];
    r2 = m->mtx[rix2];
    for (ix=0; ix<m->dim_x; ix++)
        temp = r1[ix]; r1[ix]=r2[ix]; r2[ix]=temp;
//	printf("Swap rows %d and %d\n", rix1, rix2);
//	MtxDisplay(m);
}

void MtxNormalizeRow( Matrix m, int rix, int lead)
{
    int ix;
    EL_Type *drow;
    EL_Type lv;
    drow = m->mtx[rix];
    lv = drow[lead];
    for (ix=0; ix<m->dim_x; ix++)
        drow[ix] /= lv;
//	printf("Normalize row %d\n", rix);
//	MtxDisplay(m);
}

#define MtxGet( m, rix, cix ) m->mtx[rix][cix]

void MtxToReducedREForm(Matrix m)
{
    int lead;
    int rix, iix;
    EL_Type lv;
    int rowCount = m->dim_y;

    lead = 0;
    for (rix=0; rix<rowCount; rix++) {
        if (lead >= m->dim_x)
            return;
        iix = rix;
        while (0 == MtxGet(m, iix,lead)) {
            iix++;
            if (iix == rowCount) {
                iix = rix;
                lead++;
                if (lead == m->dim_x)
                    return;
            }
        }
        MtxSwapRows(m, iix, rix );
        MtxNormalizeRow(m, rix, lead );
        for (iix=0; iix<rowCount; iix++) {
            if ( iix != rix ) {
                lv = MtxGet(m, iix, lead );
                MtxMulAndAddRows(m,iix, rix, -lv) ;
            }
        }
        lead++;
    }
}

int main()
{
    Matrix m1;
    static EL_Type r1[] = {1,2,-1,-4};
    static EL_Type r2[] = {2,3,-1,-11};
    static EL_Type r3[] = {-2,0,-3,22};
    static EL_Type *im[] = { r1, r2, r3 };

    m1 = InitMatrix( 4,3, im );
    printf("Initial\n");
    MtxDisplay(m1);
    MtxToReducedREForm(m1);
    printf("Reduced R-E form\n");
    MtxDisplay(m1);
    return 0;
}
