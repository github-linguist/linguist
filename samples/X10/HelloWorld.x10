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
 * The classic hello world program, shows how to output to the console.
 */
class HelloWorld {
  public static def main(Rail[String]) {
      Console.OUT.println("Hello World!" );
  }
}


