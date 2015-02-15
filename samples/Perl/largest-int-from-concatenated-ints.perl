sub maxnum {
    join '', sort { "$b$a" cmp "$a$b" } @_
}

print maxnum(1, 34, 3, 98, 9, 76, 45, 4), "\n";
print maxnum(54, 546, 548, 60), "\n";
