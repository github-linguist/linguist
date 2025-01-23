<?hh
/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

/**
 * This file contains a bunch of php stubs for functions that have been added
 * to hhvm (though aren't in a release yet). These are important because the
 * Hack typechecker can understand them
 */

class InvariantViolationException extends Exception {}

function invariant(mixed $test, string $message): void {
  if (!$test) {
    invariant_violation($message);
  }
}

function invariant_violation(string $message): void {
  throw new InvariantViolationException($message);
}

function class_meth(string $class, string $method) {
  return array($class, $method);
}