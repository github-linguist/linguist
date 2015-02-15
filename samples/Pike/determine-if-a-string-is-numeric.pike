int(0..1) is_number(string s)
{
    array test = array_sscanf(s, "%s%f%s");
    if (sizeof(test) == 3 && test[1] && !sizeof(test[0]) && !sizeof(test[2]) )
        return true;
    else
        return false;
}

string num = "-1.234"
is_number(num);
-> true
