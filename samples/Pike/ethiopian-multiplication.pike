int ethopian_multiply(int l, int r)
{
    int halve(int n) { return n/2; };
    int double(int n) { return n*2; };
    int(0..1) evenp(int n) { return !(n%2); };

    int product = 0;
    do
    {
        write("%5d %5d\n", l, r);
        if (!evenp(l))
            product += r;
        l = halve(l);
        r = double(r);
    }
    while(l);
    return product;
}
