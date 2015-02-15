# Random.self_init();;
- : unit = ()

# while true do
    let a = Random.int 20 in
    print_int a;
    print_newline();
    if a = 10 then raise Exit;
    let b = Random.int 20 in
    print_int b;
    print_newline()
  done;;
15
18
2
13
10
Exception: Pervasives.Exit.
