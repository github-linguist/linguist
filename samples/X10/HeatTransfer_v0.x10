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
import x10.compiler.Inline;


/**
 * This is a sample program illustrating how to use
 * X10's array classes.  It also illustrates the use
 * of foreach to acheive intra-place parallelism.
 *
 * The program solves a set of 2D partial differential
 * equations by iteratively applying a 5-point stencil
 * operation until convergence is reached.
 */
public class HeatTransfer_v0 {
    static val EPSILON = 1.0e-5;

    val N:Long;
    val A:Array_2[Double]{self!=null};
    val Tmp:Array_2[Double]{self!=null};

    public def this(size:Long) {
        N = size;
        A = new Array_2[Double](N+2, N+2);  // zero-initialized N+2 * N+2 array of doubles
        for (j in 1..N) A(0, j) = 1;     // set one border row to 1 
        Tmp = new Array_2[Double](A);
    }

    final @Inline def stencil(x:Long, y:Long):Double {
        return (A(x-1,y) + A(x+1,y) + A(x,y-1) + A(x,y+1)) / 4;
    }

    def run() {
        val is = new DenseIterationSpace_2(1,1,N,N);
        var delta:Double;
        do {
            // Compute new values, storing in tmp
            delta = Foreach.blockReduce(is,
                (i:Long, j:Long)=>{
                    Tmp(i,j) = stencil(i,j);
                    // Reduce max element-wise delta (A now holds previous values)
                    return Math.abs(Tmp(i,j) - A(i,j));
                },
                (a:Double, b:Double)=>Math.max(a,b), 0.0
            );

            // swap backing data of A and Tmp
            Array.swap(A, Tmp);
        } while (delta > EPSILON);
    }

    def prettyPrintResult() {
       for (i in 1..N) {
           for (j in 1..N) {
                Console.OUT.printf("%1.4f ",A(i,j));
            }
            Console.OUT.println();
        }
    }

    public static def main(args:Rail[String]) {
        val n = args.size > 0 ? Long.parse(args(0)) : 8;
        Console.OUT.println("HeatTransfer example with N="+n+" and epsilon="+EPSILON);
        Console.OUT.println("Initializing data structures");
        val ht = new HeatTransfer_v0(n);
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
