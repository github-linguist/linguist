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

import x10.array.DistArray_Unique;
import x10.io.Console;
import x10.util.Random;

/**
 * Calculation of an approximation to pi by using a Monte Carlo simulation
 * (throwing darts into the unit square and determining the fraction that land
 * in the unit circle).
 */
public class MontyPi {
    public static def main(args:Rail[String]) {
        if (args.size != 1L) {
            Console.OUT.println("Usage: MontyPi <number of points>");
            return;
        }
        val N = Long.parse(args(0));
        val initializer = () => {
            val r = new Random();
            var result:Long = 0;
            for(c in 1..N) {
                val x = r.nextDouble();
                val y = r.nextDouble();
                if (x*x +y*y <= 1.0) result++;
            }
            result
        };
        val result = new DistArray_Unique[Long](Place.places(), initializer);
        val pi = (4.0*result.reduce((x:Long,y:Long) => x+y, 0) as Double)/(N*Place.numPlaces());
        Console.OUT.println("The value of pi is " + pi);
    }
}
