for i in 1..10 {
        write(i);
        if i % 5 == 0 then {
                writeln();
                continue;
        }
        write(", ");
}
