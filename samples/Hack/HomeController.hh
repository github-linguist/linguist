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

require_once $_SERVER['DOCUMENT_ROOT'].'/core/controller/init.php';
require_once $_SERVER['DOCUMENT_ROOT'].'/core/controller/standard-page/init.php';
require_once $_SERVER['DOCUMENT_ROOT'].'/vendor/hhvm/xhp/src/init.php';

class HomeController extends GetController {
  use StandardPage;

  protected function getTitle(): string {
    return 'Hack Cookbook';
  }

  protected function renderMainColumn(): :xhp {
    return <div>
      <h1>Cookbook</h1>
      <p>
        The Hack Cookbook helps you write Hack code by giving you examples of
        Hack code. It is written in Hack and is open source. If you
        <a href="http://github.com/facebook/hack-example-site">
          head over to GitHub,
        </a>
        you can read the code, check out the repository, and run it
        yourself. The recipes in this cookbook are small examples that
        illustrate how to use Hack to solve common and interesting problems.
      </p>
    </div>;
  }
}
