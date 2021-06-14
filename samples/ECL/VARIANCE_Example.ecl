pointRec := { REAL x, REAL y };

analyse(ds) := MACRO

    #uniquename(stats)
    %stats% := TABLE(ds, { c := COUNT(GROUP), 
                           sx := SUM(GROUP, x), 
                           sy := SUM(GROUP, y), 
                           sxx := SUM(GROUP, x * x), 
                           sxy := SUM(GROUP, x * y), 
                           syy := SUM(GROUP, y * y), 
                           varx := VARIANCE(GROUP, x);
                           vary := VARIANCE(GROUP, y);
                           varxy := COVARIANCE(GROUP, x, y);
                           rc := CORRELATION(GROUP, x, y) });

    OUTPUT(%stats%);
    // Following should be zero
    OUTPUT(%stats%, { varx - (sxx-sx*sx/c)/c, vary - (syy-sy*sy/c)/c, varxy - (sxy-sx*sy/c)/c, rc - (varxy/SQRT(varx*vary)) });
    OUTPUT(%stats%, { 'bestFit: y=' + (STRING)((sy-sx*varxy/varx)/c) + ' + ' +(STRING)(varxy/varx)+'x' });

ENDMACRO;

ds1 := DATASET([{1, 1}, {2, 2}, {3, 3}, {4, 4}, {5, 5}, {6, 6}], pointRec);
ds2 := DATASET([ {1.93896e+009, 2.04482e+009}, 
                  {1.77971e+009, 8.54858e+008}, 
                  {2.96181e+009, 1.24848e+009}, 
                  {2.7744e+009, 1.26357e+009}, 
                  {1.14416e+009, 4.3429e+008}, 
                  {3.38728e+009, 1.30238e+009}, 
                  {3.19538e+009, 1.71177e+009} ], pointRec);

ds3 := DATASET([{1, 1.00039}, 
                {2, 2.07702}, 
                {3, 2.86158}, 
                {4, 3.87114}, 
                {5, 5.12417}, 
                {6, 6.20283} ], pointRec);

analyse(ds1);
analyse(ds2);
analyse(ds3);