/*
 *  This file is part of the X10 project (http://x10-lang.org).
 *
 *  This file is licensed to You under the Eclipse Public License (EPL);
 *  You may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *      http://www.opensource.org/licenses/eclipse-1.0.php
 *
 *  (C) Copyright IBM Corporation 2006-2014.
 *  (C) Copyright Australian National University 2011.
 */

/**
 * Compute the number of solutions to the N queens problem.
 */
public class NQueensPar {
    public static val EXPECTED_SOLUTIONS =
        [0, 1, 0, 0, 2, 10, 4, 40, 92, 352, 724, 2680, 14200, 73712, 365596, 2279184, 14772512];

    val N:Int;
    val P:Int;
    var nSolutions:Int = 0n;
    val R:IntRange;

    def this(N:Int, P:Int) { 
       this.N=N;
       this.P=P;
       this.R = 0n..(N-1n);
    }

    def start() {
        new Board().parSearch();
    }

    class Board {
        val q: Rail[Int];
        /** The number of low-rank positions that are fixed in this board for the purposes of search. */
        var fixed:Int;
        def this() {
            q = new Rail[Int](N);
            fixed = 0n;
        }

        def this(b:Board) {
            this.q = new Rail[Int](b.q);
            this.fixed = b.fixed;
        }

        /** 
         * @return true if it is safe to put a queen in file <code>j</code>
         * on the next rank after the last fixed position.
         */
        def safe(j:Int) {
            for (k in 0n..(fixed-1n)) {
                if (j == q(k) || Math.abs(fixed-k) == Math.abs(j-q(k)))
                    return false;
            }
            return true;
        }

        /** Search all positions for the current board. */
        def search() {
            for (k in R) searchOne(k);
        }

        /**
         * Modify the current board by adding a new queen
         * in file <code>k</code> on rank <code>fixed</code>,
         * and search for all safe positions with this prefix.
         */
        def searchOne(k:Int) {
            if (safe(k)) {
                if (fixed==(N-1n)) {
                    // all ranks safely filled
                    atomic NQueensPar.this.nSolutions++;
                } else {
                    q(fixed++) = k;
                    search();
                    fixed--;
                }
            }
        }

        /**
         * Search this board, dividing the work between threads
         * using a block distribution of the current free rank.
         */
        def parSearch()  {
            for (work in R.split(P)) async {
                val board = new Board(this);
                for (w in work) {
                    board.searchOne(w);
                }
            }
        }
    }

    public static def main(args:Rail[String])  {
        val n = args.size > 0 ? Int.parse(args(0)) : 8n;
        Console.OUT.println("N=" + n);
        //warmup
        //finish new NQueensPar(12, 1).start();
        val ps = [1n,2n,4n];
        for (numTasks in ps) {
            Console.OUT.println("starting " + numTasks + " tasks");
            val nq = new NQueensPar(n,numTasks);
            var start:Long = -System.nanoTime();
            finish nq.start();
            val result = (nq.nSolutions as Long)==EXPECTED_SOLUTIONS(nq.N);
            start += System.nanoTime();
            start /= 1000000;
            Console.OUT.println("NQueensPar " + nq.N + "(P=" + numTasks +
                    ") has " + nq.nSolutions + " solutions" +
                    (result? " (ok)." : " (wrong).") + "time=" + start + "ms");
        }
    }
}
