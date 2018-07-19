function rec(bottles)
{
    if (bottles > 0)
    {
        print(bottles+" bottles of beer on the wall\n")
        print(bottles+" bottles of beer\n");
        print("Take one down, pass it around\n");
        print(--bottles+" bottles of beer on the wall\n\n")
        return rec(bottles);
    }
    print("No more bottles of beer on the wall, no more bottles of beer\n");
    print("Go to the store and get some more beer, 99 bottles of beer on the wall\n");
}

rec(99);
