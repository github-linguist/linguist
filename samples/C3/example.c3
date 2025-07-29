module example;

import std::io;
import std::math;

def Callback = fn int(char* text);

enum Status : int
{
    IDLE,
    BUSY,
    DONE,
}

struct MyData
{
    char* name;
    Callback open;
    Status status;

    struct other
    {
        int value;
        int status;
    }

    union
    {
        int* number;
        char* text;
    }
}

fault MathError
{
    DIVISION_BY_ZERO
}

fn double! divide(int a, int b)
{
    if (b == 0) return MathError.DIVISION_BY_ZERO?;
    return (double)a / (double)b;
}

<*
 @param foo "the number of foos"
 @require foo > 0, foo < 1000
 @return "number of foos x 10"
 @ensure return < 10000, return > 0
*>
fn int testFoo(int foo)
{
    return foo * 10;
}

fn void example_foreach(float[] values)
{
    foreach (index, value : values)
    {
        io::printfn("%d: %f", index, value);
    }
}

fn void demo_switch(Status s)
{
    switch (s)
    {
        case IDLE:
            io::printn("Idle");
        case BUSY:
            io::printn("Busy");
            nextcase;
        case DONE:
            io::printn("Done");
    }
}

fn void test_defer(int x)
{
    defer io::printn();
    defer io::print("A");
    if (x == 1) return;
    {
        defer io::print("B");
        if (x == 0) return;
    }
    io::print("!");
}

fn int main()
{
    MyData data = { .name = "test", .status = Status.IDLE };
    test_defer(1);
    return 0;
}