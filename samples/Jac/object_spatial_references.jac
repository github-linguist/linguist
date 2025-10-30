walker Creator {
    can create with `root entry;
}

node node_a {
    has val: int;

    can make_something with Creator entry;
}

edge connector {
    has value: int = 10;
}

impl Creator.create {
    end = here;
    for i=0 to i<3 by i+=1  {
        end ++> (end := node_a(val=i));
    }
    end +>:connector:value=i:+> (end := node_a(val=i + 10));
    root <+:connector:value=i:<+ (end := node_a(val=i + 10));
    visit [-->];
}

impl node_a.make_something {
    i = 0;
    while i < 5 {
        print(f"wlecome to {self}");
        i += 1;
    }
}

with entry {
    root spawn Creator();
}
