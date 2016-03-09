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
 * Straightforward quicksort implementation using
 * naive partition-in-the-middle and not bothering with
 * well-known optimizations such as using insertion sort
 * once the partitions get small.  This is only intended
 * as a simple example of an array-based program that 
 * combines a recirsive divide and conquer algorithm 
 * with async and finish, not as a highly efficient 
 * sorting procedure..
 */
public class QSort {

  private static def partition(data:Rail[int], left:long, right:long) {
      var i:long = left;
      var j:long = right;
      var tmp:int;
      var pivot:long = data((left + right) / 2);

      while (i <= j) {
          while (data(i) < pivot) i++;
          while (data(j) > pivot) j--;
          if (i <= j) {
              tmp = data(i);
              data(i) = data(j);
              data(j) = tmp;
              i++;
              j--;
          }
      }

      return i;
  }

  public static def qsort(data:Rail[int], left:long, right:long) {
      index:long = partition(data, left, right);
      finish {
          if (left < index - 1)
              async qsort(data, left, index - 1);

          if (index < right)
              qsort(data, index, right);
      }
  }

  public static def main(args:Rail[String]) {
      val N = args.size>0 ? Long.parse(args(0)) : 100;
      val r = new x10.util.Random();
      val data = new Rail[int](N, (long)=>r.nextInt(9999n));
      qsort(data, 0, N-1);
      for (i in 0..(N-1)) {
          Console.OUT.print(data(i)); 
          if (i%10 == 9) {
	      Console.OUT.println();
          } else {
              Console.OUT.print(", ");
          }
      }
      Console.OUT.println();
  }
}

