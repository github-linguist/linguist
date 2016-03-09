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

/**
 * This is a slightly more realistic example of the
 * basic computational pattern of using async/finish
 * to express recursive divide-and-conquer algorithms.
 * The program does integration via Guassian Quadrature.
 * <p>
 * It also can serve as an example of using a closure.
 */
public class Integrate { 
  static val epsilon = 1.0e-9;

  val fun:(double)=>double;

  public def this(f:(double)=>double) { fun = f; }

  public def computeArea(left:double, right:double) {
    return recEval(left, fun(left), right, fun(right), 0);
  }

  private def recEval(l:double, fl:double, r:double, fr:double, a:double) {
    val h = (r - l) / 2;
    val hh = h / 2;
    val c = l + h;
    val fc = fun(c);
    val al = (fl + fc) * hh;   
    val ar = (fr + fc) * hh;
    val alr = al + ar;
    if (Math.abs(alr - a) < epsilon) return alr;
    val expr1:double;
    val expr2:double;
    finish {
      async { expr1 = recEval(c, fc, r, fr, ar); };
      expr2 = recEval(l, fl, c, fc, al);
    }
    return expr1 + expr2;
  }
 
  public static def main(args:Rail[String]) {
    val obj = new Integrate((x:double)=>(x*x + 1.0) * x);
    val xMax = args.size > 0 ? Long.parse(args(0)) : 10;
    val area = obj.computeArea(0, xMax);
    Console.OUT.println("The area of (x*x +1) * x from 0 to "+xMax+" is "+area);
  }
}
