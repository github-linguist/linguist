pattern = ""
for(i = 1; i <= 5; i += 1)
    {
    for(j = 1; j <= i; j += 1)
        {
        pattern += "*"
        }
    pattern += "#"
    }
show_message(pattern)
