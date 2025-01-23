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

require_once $_SERVER['DOCUMENT_ROOT'].'/core/startup/init.php';

abstract class Controller {
  protected function __construct() {
    startup();
  }

  abstract protected function getCSS(): Set<string>;
  abstract protected function getJS(): Set<string>;
  abstract protected function getTitle(): string;
  abstract protected function render(): :xhp;

  final protected function getHead(): :xhp {
    $css = $this->getCSS()->toVector()->map(
      ($css) ==> <link rel="stylesheet" type="text/css" href={$css} />
    );
    $js = $this->getJS()->toVector()->map(
      ($js) ==> <script src={$js} />
    );
    return
      <head>
      <meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
        <title>{$this->getTitle()}</title>
        {$css->toArray()}
        {$js->toArray()}
      </head>;
  }
}
