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
 * This is a small program to illustrate the use of 
 * <code>async</code> and <code>finish</code> in a 
 * prototypical recursive divide-and-conquer algorithm.  
 * It is obviously not intended to show a efficient way to
 * compute Fibonacci numbers in X10.<p>
 *
 * The heart of the example is the <code>run</code> method,
 * which directly embodies the recursive definition of 
 * <pre>
 *   fib(n) = fib(n-1)+fib(n-2);
 * </pre>
 * by using an <code>async</code> to compute <code>fib(n-1)</code> while
 * the current activity computes <code>fib(n-2)</code>.  A <code>finish</code>
 * is used to ensure that both computations are complete before 
 * their results are added together to compute <code>fib(n)</code>
 */
public class Fibonacci {

  public static def fib(n:long) {
    if (n<=2) return 1;
    
    val f1:long;
    val f2:long;
    finish {
      async { f1 = fib(n-1); }
      f2 = fib(n-2);
    }
    return f1 + f2;
  }

  public static def main(args:Rail[String]) {
    val n = (args.size > 0) ? Long.parse(args(0)) : 10;
    Console.OUT.println("Computing fib("+n+")");
    val f = fib(n);
    Console.OUT.println("fib("+n+") = "+f);
  }
}

