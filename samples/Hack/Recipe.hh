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
require_once $_SERVER['DOCUMENT_ROOT'].'/core/myxhp/init.php';

abstract class Recipe extends GetController {
  use StandardPage;

  abstract protected function getName(): string;
  abstract protected function getFilenames(): Vector<string>;
  abstract protected function getDocs(): Vector<(string, string)>;

  protected function getDescription(): ?string {
    return null;
  }

  final protected function getTitle(): string {
    return $this->getName().' - Hack Cookbook';
  }

  final protected function renderMainColumn(): :xhp {
    $main_column =
      <x:frag>
        <h1>{$this->getName()}</h1>
      </x:frag>;
    $description = $this->getDescription();
    if ($description !== null) {
      $main_column->appendChild(<p>{$description}</p>);
    }
    foreach ($this->getFilenames() as $filename) {
      $file =
        <div class="file">
          <div class="filename">{$filename}</div>
          <phpfile filename={$filename}/>
        </div>;
      $main_column->appendChild($file);
    }
    $recipe = $this;
    if ($recipe instanceof RecipeWithDemo) {
      try {
        $result = $recipe->getDemoResult();
      } catch (Exception $e) {
        $result = sprintf(
          "Demo threw an %s:\n%s",
          get_class($e),
          $e->getMessage(),
        );
      }
      $result = explode("\n", trim($result));
      $result = array_map($x ==> <x:frag>{$x}<br/></x:frag>, $result);
      $demo =
        <x:frag>
          <div class="demo" id="demo">
            <h3>Demo</h3>
            {$recipe->getDemoXHP()}
            <div class="filename">{$recipe->getDemoFilename()}</div>
            <phpfile filename={$recipe->getDemoFilename()}/>
            <div class="filename">Output</div>
            <div class="demoResult">
              {$result}
            </div>
          </div>
        </x:frag>;
      $main_column->appendChild($demo);
    }
    if (!$this->getDocs()->isEmpty()) {
      $render_doc_link = function($doc) {
        list($name, $link) = $doc;
        $link = "http://hhvm.com/manual/en/$link.php";
        return <li><a href={$link}>{$name}</a></li>;
      };
      $main_column->appendChild(
        <div class="docs">
          <h3>Relevant Official Documentation</h3>
          <ul>
            {$this->getDocs()->map($render_doc_link)->toArray()}
          </ul>
        </div>
      );
    }
    return $main_column;
  }
}
