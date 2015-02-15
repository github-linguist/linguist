package diningphilosophers;

import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

enum PhilosopherState { Get, Eat, Pon }

class Fork {
    public static final int ON_TABLE = -1;
    static int instances = 0;
    public int id;
    public AtomicInteger holder = new AtomicInteger(ON_TABLE);

    Fork() { id = instances++; }
}

class Philosopher implements Runnable {
    static final int maxWaitMs = 100;                          //  must be > 0
    static AtomicInteger token = new AtomicInteger(0);
    static int instances = 0;
    static Random rand = new Random();
    AtomicBoolean end = new AtomicBoolean(false);
    int id;
    PhilosopherState state = PhilosopherState.Get;
    Fork left;
    Fork right;
    int timesEaten = 0;

    Philosopher() {
        id = instances++;
        left = Main.forks.get(id);
        right = Main.forks.get((id+1)%Main.philosopherCount);
    }

    void sleep() { try { Thread.sleep(rand.nextInt(maxWaitMs)); }
        catch (InterruptedException ex) {} }

    void waitForFork(Fork fork) {
        do {
            if (fork.holder.get() == Fork.ON_TABLE) {
                fork.holder.set(id);                //  my id shows I hold it
                return;
            } else {                                //  someone still holds it
                sleep();                            //  check again later
            }
        } while (true);
    }

    public void run() {
        do {
            if (state == PhilosopherState.Pon) {    //  all that pondering
                state = PhilosopherState.Get;       //  made me hungry
            } else { // ==PhilosopherState.Get
                if (token.get() == id) {            //  my turn now
                    waitForFork(left);
                    waitForFork(right);             //  Ah needs me some foahks!
                    token.set((id+2)% Main.philosopherCount);
                    state = PhilosopherState.Eat;
                    timesEaten++;
                    sleep();                        //  eat for a while
                    left.holder.set(Fork.ON_TABLE);
                    right.holder.set(Fork.ON_TABLE);
                    state = PhilosopherState.Pon;   //  ponder for a while
                    sleep();
                } else {                    //  token.get() != id, so not my turn
                    sleep();
                }
            }
        } while (!end.get());
    }
}

public class Main {
    static final int philosopherCount = 5; //  token +2 behavior good for odd #s
    static final int runSeconds = 15;
    static ArrayList<Fork> forks = new ArrayList<Fork>();
    static ArrayList<Philosopher> philosophers = new ArrayList<Philosopher>();

    public static void main(String[] args) {
        for (int i = 0 ; i < philosopherCount ; i++) forks.add(new Fork());
        for (int i = 0 ; i < philosopherCount ; i++)
            philosophers.add(new Philosopher());
        for (Philosopher p : philosophers) new Thread(p).start();
        long endTime = System.currentTimeMillis() + (runSeconds * 1000);

        do {                                                    //  print status
            StringBuilder sb = new StringBuilder("|");

            for (Philosopher p : philosophers) {
                sb.append(p.state.toString());
                sb.append("|");            //  This is a snapshot at a particular
            }                              //  instant.  Plenty happens between.

            sb.append("     |");

            for (Fork f : forks) {
                int holder = f.holder.get();
                sb.append(holder==-1?"   ":String.format("P%02d",holder));
                sb.append("|");
            }

            System.out.println(sb.toString());
            try {Thread.sleep(1000);} catch (Exception ex) {}
        } while (System.currentTimeMillis() < endTime);

        for (Philosopher p : philosophers) p.end.set(true);
        for (Philosopher p : philosophers)
            System.out.printf("P%02d: ate %,d times, %,d/sec\n",
                p.id, p.timesEaten, p.timesEaten/runSeconds);
    }
}
