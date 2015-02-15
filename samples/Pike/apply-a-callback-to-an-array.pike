int cube(int n)
{
    return n*n*n;
}

array(int) a = ({ 1,2,3,4,5 });
array(int) b = cube(a[*]);      // automap operator
array(int) c = map(a, cube);    // conventional map function
