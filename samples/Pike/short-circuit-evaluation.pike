int(0..1) a(int(0..1) i)
{
    write(" a\n");
    return i;
}

int(0..1) b(int(0..1) i)
{
    write(" b\n");
    return i;
}

foreach(({ ({ false, false }),  ({ false, true }), ({ true, true }), ({ true, false }) });; array(int) args)
{
    write(" %d && %d\n", @args);
    a(args[0]) && b(args[1]);

    write(" %d || %d\n", @args);
    a(args[0]) || b(args[1]);
}
