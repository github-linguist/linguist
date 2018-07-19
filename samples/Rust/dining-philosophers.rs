extern mod extra;
use std::rt::io::timer;
use extra::comm::DuplexStream;

fn phil(phil: ~str, diner: &DuplexStream<int, int>, firstChopstick: int, secondChopstick: int)
{
    let mut sleep_time: u64;
    print(fmt!("%s sat down\n", phil));
    for _ in range(1,3)
    {
        print(fmt!("%s is thinking\n", phil));
        sleep_time = std::rand::random();
        timer::sleep((sleep_time%5)*500);
        print(fmt!("%s is hungry\n", phil));
        //get left chopstick
        diner.send(firstChopstick);
        let mut recv: int = diner.recv();
        while recv == 0
        {
            diner.send(firstChopstick);
            recv = diner.recv();
        }
        print(fmt!("%s picked up his left chopstick\n", phil));
        //get right chopstick
        diner.send(secondChopstick);
        recv = diner.recv();
        while recv == 0
        {
            diner.send(secondChopstick);
            recv = diner.recv();
        }
        print(fmt!("%s picked up his right chopstick\n", phil));
        //eat
        print(fmt!("%s is eating...\n", phil));
        sleep_time = std::rand::random();
        timer::sleep((sleep_time%3)*500);
        print(fmt!("%s is done eating\n", phil));
        //set down left chopstick
        print(fmt!("%s set down his left chopstick\n", phil));
        diner.send(-1*firstChopstick);
        //set down right chopstick
        print(fmt!("%s set down his right chopstick\n", phil));
        diner.send(-1*secondChopstick);

    }
    diner.send(0);
    diner.recv();
    print(fmt!("%s has exited\n", phil));
}

fn main()
{
    //set the table:
    //false means chopstick is on the table
    //true means chopstick is taken
    let mut chopsticks: ~[bool] = ~[false, false, false, false, false];

    //diner_ will try to take resources from the table, host_
    //will respond with whether that action was successful.
    let (diner1, host1) = DuplexStream();
    let (diner2, host2) = DuplexStream();
    let (diner3, host3) = DuplexStream();
    let (diner4, host4) = DuplexStream();
    let (diner5, host5) = DuplexStream();

    //Make the first 4 "right-handed" philosophers
    do spawn{ phil(~"Hobbes", &diner1, 1, 2); }
    do spawn{ phil(~"Locke", &diner2, 2, 3); }
    do spawn{ phil(~"Machiavelli", &diner3, 3, 4); }
    do spawn{ phil(~"Montesquieu", &diner4, 4, 5); }
    //Make the last, "left-handed" philosopher
    do spawn{ phil(~"Rousseau", &diner5, 1, 5); }

    //keep track of number of people still at the table
    let mut remaining = 5;
    while remaining > 0
    {
        matchReq(&mut chopsticks, &host1, &mut remaining);
        matchReq(&mut chopsticks, &host2, &mut remaining);
        matchReq(&mut chopsticks, &host3, &mut remaining);
        matchReq(&mut chopsticks, &host4, &mut remaining);
        matchReq(&mut chopsticks, &host5, &mut remaining);
        std::task::deschedule();
   }
}

fn matchReq(chopsticks: &mut ~[bool], host: &DuplexStream<int, int>, remaining: &mut int) {
    if (host.peek())
    {
        let from = host.try_recv();
        match from
        {
            Some(0) => { *remaining += -1; host.try_send(0); return; },
            Some(x) if x > 0 => { if(chopsticks[x-1]) { host.send(0); } else { chopsticks[x-1] = true; host.send(1); } },
            Some(x) => { chopsticks[(-x)-1] = false; },
            None => { *remaining += -1; host.try_send(0); return; }
        }
    }
}
