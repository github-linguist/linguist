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

import x10.io.Console;

/**
 * A simple illustration of loop parallelization within a single place.
 */
public class ArraySum {

    var sum:Long;
    val data:Rail[Long];

    public def this(n:Long) {
	// Create a Rail with n elements (0..(n-1)), all initialized to 1.
        data = new Rail[Long](n, 1);
        sum = 0;
    }

    def sum(a:Rail[Long], start:Long, last:Long) {
        var mySum: Long = 0;
        for (i in start..(last-1)) { 
        	mySum += a(i);
        }
        return mySum;
    }

    def sum(numThreads:Long) {
        val mySize = data.size/numThreads;
        finish for (p in 0..(numThreads-1)) async {
            val mySum = sum(data, p*mySize, (p+1)*mySize);
            // Multiple activities will simultaneously update
            // this location -- so use an atomic operation.
            atomic sum += mySum;
        }
    }
    
    public static def main(args:Rail[String]) {
        var size:Long = 5*1000*1000;
        if (args.size >=1)
            size = Long.parse(args(0));

        Console.OUT.println("Initializing.");
        val a = new ArraySum(size);
        val P = [1,2,4];

        //warmup loop
        Console.OUT.println("Warming up.");
        for (numThreads in P)
            a.sum(numThreads);
        
        for (numThreads in P) {
            Console.OUT.println("Starting with " + numThreads + " threads.");
            a.sum=0;
            var time: long = - System.nanoTime();
            a.sum(numThreads);
            time += System.nanoTime();
            Console.OUT.println("For p=" + numThreads
                    + " result: " + a.sum 
                    + ((size==a.sum)? " ok" : "  bad") 
                    + " (time=" + (time/(1000*1000)) + " ms)");
        }
    }
}
