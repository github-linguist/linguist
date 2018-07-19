bottles := method(i,
    if(i==0, return "no more bottles of beer")
    if(i==1, return "1 bottle of beer")
    "" .. i .. " bottles of beer"
)
for(i, 99, 1, -1,
    write(
        bottles(i), " on the wall, ",
        bottles(i), ",\n",
        "take one down, pass it around,\n",
        bottles(i - 1), " on the wall.\n\n"
    )
)
