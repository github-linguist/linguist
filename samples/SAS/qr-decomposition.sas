/* See http://support.sas.com/documentation/cdl/en/imlug/63541/HTML/default/viewer.htm#imlug_langref_sect229.htm */

proc iml;
a={12 -51 4,6 167 -68,-4 24 -41};
print(a);
call qr(q,r,p,d,a);
print(q);
print(r);
quit;

/*
                  a

           12       -51         4
            6       167       -68
           -4        24       -41


                  q

    -0.857143 0.3942857 -0.331429
    -0.428571 -0.902857 0.0342857
    0.2857143 -0.171429 -0.942857


                  r

          -14       -21        14
            0      -175        70
            0         0        35

*/
