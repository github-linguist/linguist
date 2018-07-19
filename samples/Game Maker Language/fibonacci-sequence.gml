///fibonacci(n)
//Returns the nth fibonacci number

var n, numb;
n = argument0;

if (n == 0)
    {
    numb = 0;
    }
else
    {
    var fm2, fm1;
    fm2 = 0;
    fm1 = 1;
    numb = 1;
    repeat(n-1)
        {
        numb = fm2+fm1;
        fm2 = fm1;
        fm1 = numb;
        }
    }

return numb;
