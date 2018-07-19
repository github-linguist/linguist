string input = "a!===b=!=c";
array sep = ({"==", "!=", "=" });

array result = replace(input, sep, `+("\0", sep[*], "\0"))/"\0";
result;
Result: ({ "a", "!=", "", "==", "b", "=", "", "!=", "c" })

int pos = 0;
foreach(result; int index; string data)
{
    if ((<"==", "!=", "=">)[data])
        result[index] = ({ data, pos });
    pos+=sizeof(data);
}

result;
Result: ({"a", ({"!=", 1}), "", ({"==", 3}), "b", ({"=", 6}), "", ({"!=", 7}), "c"})
