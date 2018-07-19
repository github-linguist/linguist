class CatchAll
{
    mixed `->(string name)
    {
        return lambda(int arg){ write("you are calling %s(%d);\n", name, arg); };
    }
}

> CatchAll()->hello(5);
you are calling hello(5);
> CatchAll()->something(99);
you are calling something(99);
