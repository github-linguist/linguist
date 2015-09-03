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

public class Histogram {
    public static def compute(data:Rail[Int], numBins:Int) {
        val bins = new Rail[Int](numBins);
        finish for (i in data.range) async {
           val b = data(i) % numBins;
           atomic bins(b)++;
        }
        return bins;
    }

    public static def run(N:Int, S:Int):Boolean {
        val a = new Rail[Int](N, (i:long)=> i as int);
        val b = compute(a, S);
        val v = b(0);
        var ok:Boolean = true;
        for (x in b.range) ok &= (b(x)==v);
        return ok;
    }

    public static def main(args:Rail[String]) {
        if (args.size != 2L) {
            Console.OUT.println("Usage: Histogram SizeOfArray NumberOfBins");
            return;
        }
        val N = Int.parse(args(0));
        val S = Int.parse(args(1));
        val ok = run(N,S);
        if (ok) {
            Console.OUT.println("Test ok.");
        } else {
            Console.OUT.println("Test failed.");
        }
    }
}
