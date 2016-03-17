/*
 *  This file is part of the X10 project (http://x10-lang.org).
 *
 *  This file is licensed to You under the Eclipse Public License (EPL);
 *  You may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *      http://www.opensource.org/licenses/eclipse-1.0.php
 *
 *  (C) Copyright IBM Corporation 2006-2014.
 */

import x10.array.*;
import x10.compiler.Foreach;
import x10.util.Team;

/**
 * This is a sample program illustrating how to use
 * X10's distributed array classes.  It also illustrates the use
 * of foreach to achieve intra-place parallelism and the mixture
 * of APGAS finish/async/at with Team collective operations.
 *
 * This version of the program uses a vanilla DistArray without
 * ghost regions.  As a result, the stencil function does
 * inefficient fine-grained neighbor communication to get individual values.
 * Compare this to HeatTransfer_v2 which utilizes ghost regions and
 * bulk ghost-region exchange functions.
 * 
 * The program solves a set of 2D partial differential
 * equations by iteratively applying a 5-point stencil
 * operation until convergence is reached.
 */
public class HeatTransfer_v1 {
    static val EPSILON = 1.0e-5;

    val N:Long;
    val A:DistArray_BlockBlock_2[Double]{self!=null};
    val Tmp:DistArray_BlockBlock_2[Double]{self!=null};

    public def this(size:Long) {
        N = size;
        val init = (i:Long, j:Long)=>i==0 ? 1.0 : 0.0;
        A = new DistArray_BlockBlock_2[Double](N+2, N+2, init);
        Tmp = new DistArray_BlockBlock_2[Double](N+2, N+2, init);
    }

    final def stencil(x:Long, y:Long):Double {
        val cls = (dx:Long, dy:Long)=>{
            val p = A.place(x+dx, y+dy);
            p == here ? A(x+dx,y+dy) : at (p) A(x+dx,y+dy)
        };
        val tmp = cls(-1,0) + cls(1,0) + cls(0,-1) + cls(0,1);
        return tmp / 4; 
    }

    def run() {
        val myTeam = new Team(A.placeGroup());
        finish for (p in A.placeGroup()) at (p) async {
            // Compute the subset of the local indices on which
            // we want to apply the stencil (the interior points of the N+2 x N+2 grid)
            val li = A.localIndices();
            val interior = new DenseIterationSpace_2(li.min(0) == 0 ? 1 : li.min(0),
                                                     li.min(1) == 0 ? 1 : li.min(1),
                                                     li.max(0) == N+1 ? N : li.max(0),
                                                     li.max(1) == N+1 ? N : li.max(1));
            var delta:Double;
            do {
                // Compute new values, storing in tmp
                val myDelta = Foreach.blockReduce(interior,
                    (i:Long, j:Long)=>{
                        Tmp(i,j) = stencil(i,j);
                        // Reduce max element-wise delta (A now holds previous values)
                        return Math.abs(Tmp(i,j) - A(i,j));
                    },
                    (a:Double, b:Double)=>Math.max(a,b), 0.0
                );

                myTeam.barrier();

                // Unlike Array, DistArray doesn't provide an optimized swap.
                // So, until it does, we have to copy the data elements.
                Foreach.block(interior, (i:Long, j:Long)=>{
                    A(i,j) = Tmp(i,j);
                });

                delta = myTeam.allreduce(myDelta, Team.MAX);
            } while (delta > EPSILON);
        }
    }
 
   def prettyPrintResult() {
       for (i in 1..N) {
           for (j in 1..N) {
                val x = at (A.place(i,j)) A(i,j);
                Console.OUT.printf("%1.4f ", x);
            }
            Console.OUT.println();
        }
    }

    public static def main(args:Rail[String]) {
        val n = args.size > 0 ? Long.parse(args(0)) : 8;
        Console.OUT.println("HeatTransfer example with N="+n+" and epsilon="+EPSILON);
        Console.OUT.println("Initializing data structures");
        val ht = new HeatTransfer_v1(n);
        Console.OUT.println("Beginning computation...");
        val start = System.nanoTime();
        ht.run();
        val stop = System.nanoTime();
        Console.OUT.printf("...completed in %1.3f seconds.\n", ((stop-start) as double)/1e9);
        if (n <= 10) {
            ht.prettyPrintResult();
        }
    }
}
