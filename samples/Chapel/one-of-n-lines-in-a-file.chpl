use Random;

proc one_of_n(n) {
    var rand = new RandomStream();
    var keep = 1;

    for i in 2..n do
        if rand.getNext() < 1.0 / i then
            keep = i;

    delete rand;

    return keep;
}
