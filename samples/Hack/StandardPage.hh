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

trait StandardPage {
  require extends Controller;

  abstract protected function renderMainColumn(): :xhp;

  protected function getExtraCSS(): Set<string> {
    return Set {};
  }

  protected function getExtraJS(): Set<string> {
    return Set {};
  }

  final protected function getCSS(): Set<string> {
    return (Set {
      '/css/base.css',
    })->addAll($this->getExtraCSS());
  }

  final protected function getJS(): Set<string> {
    return (Set {
    })->addAll($this->getExtraJS());
  }

  final private function renderHeader(): :xhp {
    return
      <div class="hackHeader">
        <div class="width">
          <a href="http://hacklang.org/">
            <div class="logo">Hack</div>
          </a>
          <div class="headerNav">
          <ul>
            <li>
              <a href="http://hacklang.org/install/">Install</a>
            </li>
            <li>
              <a href="http://hacklang.org/tutorial/">Tutorial</a>
            </li>
            <li>
              <a href="/">Cookbook</a>
            </li>
            <li>
              <a href="http://hhvm.com/manual">Docs</a>
            </li>
            <li>
              <a href="http://github.com/facebook/hhvm">GitHub</a>
            </li>
            <li>
              <a href="http://hhvm.com/">HHVM</a>
            </li>
          </ul>
          </div>
        </div>
      </div>;
  }

  final protected function render(): :xhp {
    return
      <div>
        {$this->renderHeader()}
        <div class="width">
          <div class="mainContainer">
            <div class="mainColumn">{$this->renderMainColumn()}</div>
            <hack:nav/>
          </div>
        </div>
    </div>;
  }
}
