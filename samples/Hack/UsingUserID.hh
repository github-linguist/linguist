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

function get_something_string(ID $id, string $something): string {
  return sprintf("Awesome %s #%d\n", $something, id_to_int($id));
}

function get_user_string(USER_ID $id): string {
  return get_something_string($id, 'user');
}

function get_cow_string(COW_ID $id): string {
  return get_something_string($id, 'cow');
}
