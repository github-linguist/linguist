function LCM(A)  // A is an integer array (e.g. [-50,25,-45,-18,90,447])
{
    var n = A.length, a = Math.abs(A[0]);
    for (var i = 1; i < n; i++)
     { var b = Math.abs(A[i]), c = a;
       while (a && b){ a > b ? a %= b : b %= a; }
       a = Math.abs(c*A[i])/(a+b);
     }
    return a;
}

/* For example:
   LCM([-50,25,-45,-18,90,447]) -> 67050
*/
