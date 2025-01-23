<?hh // strict
/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

final class AssertException extends Exception {}

final class Assert {
  public static function isNum(mixed $x): num {
    if (is_float($x)) {
      return $x;
    } else if (is_int($x)) {
      return $x;
    }
    throw new AssertException('Expected an int or float value');
  }

  public static function isInt(mixed $x): int {
    if (is_int($x)) {
      return $x;
    }
    throw new AssertException('Expected an int');
  }

  public static function isFloat(mixed $x): float {
    if (is_float($x)) {
      return $x;
    }
    throw new AssertException('Expected a float');
  }

  public static function isString(mixed $x): string {
    if (is_string($x)) {
      return $x;
    }
    throw new AssertException('Expected a string');
  }

  // For arrays you need to check every element
  public static function isArrayOf<T>(
    (function(mixed): T) $fn,
    mixed $x,
  ): array<T> {
    if (is_array($x)) {
      return array_map($fn, $x);
    }
    throw new AssertException('Expected an array');
  }
}
