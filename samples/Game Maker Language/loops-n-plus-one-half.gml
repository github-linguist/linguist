str = ""
for(i = 1; i <= 10; i += 1)
    {
    str += string(i)
    if(i != 10)
        str += ", "
    }
show_message(str)
