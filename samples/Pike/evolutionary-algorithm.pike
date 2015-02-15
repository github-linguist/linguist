string chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";

string mutate(string data, int rate)
{
    array(int) alphabet=(array(int))chars;
    multiset index = (multiset)enumerate(sizeof(data));
    while(rate)
    {
        int pos = random(index);
        data[pos]=random(alphabet);
        rate--;
    }
    return data;
}

int fitness(string input, string target)
{
    return `+(@`==(((array)input)[*], ((array)target)[*]));
}

void main()
{
    array(string) alphabet = chars/"";
    string target = "METHINKS IT IS LIKE A WEASEL";
    string parent = "";

    while(sizeof(parent) != sizeof(target))
    {
        parent += random(alphabet);
    }

    int count;
    write(" %5d: %s\n", count, parent);
    while (parent != target)
    {
        string child = mutate(parent, 2);
        count++;
        if (fitness(child, target) > fitness(parent, target))
        {
            write(" %5d: %s\n", count, child);
            parent = child;
        }
    }
}
