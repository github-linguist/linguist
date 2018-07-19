#!/usr/bin/env pike

int next(int n)
{
    if (n==1)
        return 0;
    if (n%2)
        return 3*n+1;
    else
        return n/2;
}

array(int) hailstone(int n)
{
    array seq = ({ n });
    while (n=next(n))
        seq += ({ n });
    return seq;
}

void main()
{
    array(int) two = hailstone(27);
    if (equal(two[0..3], ({ 27, 82, 41, 124 })) && equal(two[<3..], ({ 8,4,2,1 })))
        write("sizeof(({ %{%d, %}, ... %{%d, %} }) == %d\n", two[0..3], two[<3..], sizeof(two));

    mapping longest = ([ "length":0, "start":0 ]);

    foreach(allocate(100000); int start; )
    {
        int length = sizeof(hailstone(start));
        if (length > longest->length)
        {
            longest->length = length;
            longest->start = start;
        }
    }
    write("longest sequence starting at %d has %d elements\n", longest->start, longest->length);
}
