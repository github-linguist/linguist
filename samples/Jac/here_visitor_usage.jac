node MyNode{
    has val:int;

    can ability1 with MyWalker entry {
        print("Visitor name is ",visitor.name);
    }
}

walker MyWalker{
    has name:str;

    can ability2 with MyNode entry {
        print("Here value is ",here.val);
    }
}

with entry {
    Node1 = MyNode(10);
    Walker1 = MyWalker("Walker 1");
    Walker1 spawn Node1;
}