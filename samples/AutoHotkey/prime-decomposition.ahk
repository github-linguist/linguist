MsgBox % factor(8388607)   ; 47 * 178481

factor(n)
{
    if (n = 1)
        return
    f = 2
    while (f <= n)
    {
        if (Mod(n, f) = 0)
        {
            next := factor(n / f)
            return, % f "`n" next
        }
        f++
    }
}
