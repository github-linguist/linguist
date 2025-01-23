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

final class :phpfile extends :x:primitive {
  category %flow;

  attribute string filename;

  /**
   * Ok, I'll admit this is kind of gross. I don't really want to implement
   * syntax highlighting, so I'm relying on the built-in PHP support. XHP
   * makes html strings sort of difficult to use (which is good cause they're
   * dangerous). Anyway, this is one way around it :)
   */
  protected function stringify(): string {
    return
      '<div class="code">'.
      (string)highlight_file($this->getAttribute('filename'), /*ret*/ true).
      '</div>';
  }
}
