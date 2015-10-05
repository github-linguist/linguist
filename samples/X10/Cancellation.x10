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

import x10.xrx.Runtime;

/**
 * Demonstrate how to instantiate the X10 runtime as an executor service
 * submit jobs to the runtime, wait jobs to complete and cancel all jobs
 * 
 * Compile with: x10c -O -EXECUTOR_MODE=true Cancellation.x10
 * Run with:     X10_CANCELLABLE=true X10_NPLACES=4 x10 -DX10RT_IMPL=JavaSockets Cancellation
 */
class Cancellation {
    static def job(id:Long, iterations:Long) = ()=>{
        at (Place.places().next(here)) async {
            for (i in 1..iterations) {
                finish for (p in Place.places()) { 
                    at (p) async Console.OUT.println(here+" says hello (job " + id + ", iteration " + i + ")");
                }
                Console.ERR.println();
                System.sleep(200);
            }
        }
    };

    public static def main(args:Rail[String]):void {
        val w1 = Runtime.submit(job(1, 5));
        w1.await(); Console.ERR.println("Job 1 completed\n");
        val w2 = Runtime.submit(job(2, 1000));
        System.threadSleep(1000);
        val c1 = Runtime.cancelAll();
        try { w2.await(); } catch (e:Exception) { Console.ERR.println("Job 2 aborted with exception " + e +"\n"); }
        c1.await(); // waiting for cancellation to be processed
        System.threadSleep(1000);
        Runtime.submit(job(3, 1000));
        Runtime.submit(job(4, 1000));
        System.threadSleep(1000);
        val c2 = Runtime.cancelAll();
        c2.await();
        Console.ERR.println("Goodbye\n");
    }
}
