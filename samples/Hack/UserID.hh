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
require_once $_SERVER['DOCUMENT_ROOT'].'/core/funs/init.php';

// Outside of this file, no one knows that these types are ints. They do know
// that USER_ID is an ID and COW_ID is an ID
newtype ID = int;
newtype USER_ID as ID = ID;
newtype COW_ID as ID = ID;

function assert_user_id(int $x): USER_ID {
  // Everyone knows all user ids are odd
  invariant($x % 2, sprintf('Invalid user ID: %d', $x));
  return $x;
}

function assert_cow_id(int $x): COW_ID {
  // Everyone knows all cow ids are even
  invariant($x % 2 === 0, sprintf('Invalid cow ID: %d', $x));
  return $x;
}

function id_to_int(ID $id): int {
  return $id;
}
