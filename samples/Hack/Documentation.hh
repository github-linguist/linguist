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

require_once $_SERVER['DOCUMENT_ROOT'].'/vendor/hhvm/xhp/src/init.php';

final class :documentation extends :x:element {
  attribute string name;

  protected function render(): :xhp {
    $name = implode('.', explode(' ', $this->getAttribute('name'))).".php";
    $href = "http://hhvm.com/manual/en/$name";
    return <a class="docs button" href={$href} target="_blank">docs &rarr;</a>;
  }
}
