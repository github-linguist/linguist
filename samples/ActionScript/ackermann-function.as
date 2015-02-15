public function ackermann(m:uint, n:uint):uint
{
    if (m == 0)
    {
        return n + 1;
    }
    if (n == 0)
    {
        return ackermann(m - 1, 1);
    }		

    return ackermann(m - 1, ackermann(m, n - 1));
}
