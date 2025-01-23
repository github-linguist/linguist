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

abstract class GetController extends Controller {
  final protected function __construct(private Request $request) {
    parent::__construct();
  }

  final protected function getRequest(): Request {
    return $this->request;
  }

  final public function go(array<mixed, mixed> $get): void {
    $request = new Request(Map::fromArray($get));
    $controller = new static($request);
    echo "<!DOCTYPE html>";
    $head = $controller->getHead();
    $body = $controller->render();
    echo (string)$head;
    echo (string)$body;
  }
}
