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

function getGETParams(): Map<string, mixed> {
  // $_GET is not defined in code so Hack doesn't know about it and you can't
  // use it in strict mode. You can interact with it outside of strict mode,
  // though.
  return Map::fromArray($_GET);
}

function getPOSTParams(): Map<string, mixed> {
  // Same deal with $_POST and other magically defined globals
  return Map::fromArray($_POST);
}

// Same deal with $_SERVER
function isGET(): bool {
  return $_SERVER['REQUEST_METHOD'] === 'GET';
}
