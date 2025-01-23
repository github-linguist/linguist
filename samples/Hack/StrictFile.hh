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

abstract class MyRequest {
  abstract public function getParams(): Map<string, mixed>;

  // Throws when things go wrong
  public function intParamX(string $name): int {
    $params = $this->getParams();
    invariant($params->contains($name), sprintf('Unknown param: %s', $name));
    $param = $params[$name];
    invariant(is_numeric($param), sprintf('Param %s is not an int', $name));
    return (int)$param;
  }

  // A lenient version
  public function intParam(string $name): ?int {
    $params = $this->getParams();
    if (!$params->contains($name)) { return null; }
    $param = $params[$name];
    if (!is_numeric($param)) { return null; }
    return (int)$param;
  }
}

final class MyGETRequest extends MyRequest {
  public function getParams(): Map<string, mixed> {
    return getGETParams();
  }
}

final class MyPOSTRequest extends MyRequest {
  public function getParams(): Map<string, mixed> {
    return getPOSTParams();
  }
}
