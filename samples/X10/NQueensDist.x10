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

import x10.array.DistArray_Unique;

/**
 * A distributed version of NQueens. Runs over NUM_PLACES.
 * Identical to NQueensPar, except that work is distributed 
 * over multiple places rather than shared between threads.
 */
public class NQueensDist {
    public static val EXPECTED_SOLUTIONS =
        [0, 1, 0, 0, 2, 10, 4, 40, 92, 352, 724, 2680, 14200, 73712, 365596, 2279184, 14772512];

    val N:Long;
    val P:Long;
    val results:DistArray_Unique[Long];
    val R:LongRange;

    def this(N:Long, P:Long) { 
        this.N=N;
        this.P=P;
        this.results = new DistArray_Unique[Long]();
        this.R = 0..(N-1);
    }
    def start() {
        new Board().distSearch();
    }
    def run():Long {
       finish start();
       val result = results.reduce(((x:Long,y:Long) => x+y),0);
       return result;
    }

    class Board {
        val q: Rail[Long];
        /** The number of low-rank positions that are fixed in this board for the purposes of search. */
        var fixed:Long;
        def this() {
            q = new Rail[Long](N);
            fixed = 0;
        }

        /** 
         * @return true if it is safe to put a queen in file <code>j</code>
         * on the next rank after the last fixed position.
         */
        def safe(j:Long) {
            for (k in 0..(fixed-1)) {
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
        def searchOne(k:Long) {
            if (safe(k)) {
                if (fixed==(N-1)) {
                    // all ranks safely filled
                    atomic NQueensDist.this.results(here.id)++;
                } else {
                    q(fixed++) = k;
                    search();
                    fixed--;
                }
            }
        }

        /**
         * Search this board, dividing the work between all places
         * using a block distribution of the current free rank.
         */
        def distSearch()  {
            val work = R.split(Place.numPlaces());
            finish for (p in Place.places()) {
                val myPiece = work(p.id);
                at (p) async {
                    // implicit copy of 'this' made across the at divide
                    for (k in myPiece) {
                        searchOne(k);
                    }
                }
            }
        }
    }

    public static def main(args:Rail[String])  {
        val n = args.size > 0 ? Long.parse(args(0)) : 8;
        Console.OUT.println("N=" + n);
        //warmup
        //finish new NQueensPar(12, 1).start();
        val P = Place.numPlaces();
        val nq = new NQueensDist(n,P);
        var start:Long = -System.nanoTime();
        val answer = nq.run();
        val result = answer==EXPECTED_SOLUTIONS(n);
        start += System.nanoTime();
        start /= 1000000;
        Console.OUT.println("NQueensDist " + nq.N + "(P=" + P +
                            ") has " + answer + " solutions" +
                            (result? " (ok)." : " (wrong).") + 
                            "time=" + start + "ms");
    }
}
