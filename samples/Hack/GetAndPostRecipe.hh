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

require_once $_SERVER['DOCUMENT_ROOT'].'/core/controller/recipe/init.php';
require_once "demo.php";

class GetAndPostRecipe extends Recipe implements RecipeWithDemo {

  protected function getName(): string {
    return '$_GET and $_POST';
  }

  <<Override>>
  protected function getDescription(): ?string {
    return 'A small example of how to interact with superglobals and the '.
      'untyped data they can contain.';
  }

  protected function getFilenames(): Vector<string> {
    return Vector {
      'NonStrictFile.php',
      'StrictFile.php',
    };
  }

  protected function getDocs(): Vector<(string, string)> {
    return Vector {
      tuple('invariant()', 'hack.otherrulesandfeatures.invariant'),
    };
  }

  public function getDemoFilename(): string {
    return 'demo.php';
  }

  public function getDemoResult(): string {
    return get_and_post_main();
  }

  public function getDemoXHP(): :xhp {
    $url = '/recipes/get-and-post/';
    return
      <x:frag>
        <div>
          <a href={"$url?myIntParam=8675309#demo"} class="button">GET myIntParam=8675309</a>
        </div>
        <div>
        <a href={"$url?myIntParam=boom#demo"} class="button">GET myIntParam=boom</a>
        </div>
        <div>
        <form action={"$url#demo"} method="post">
          <input type="hidden" name="myIntParam" value="5551234"/>
          <input type="submit" class="button" value="POST myIntParam=5551234"/>
        </form>
        </div>
        <div>
        <form action={"$url#demo"} method="post">
          <input type="hidden" name="myIntParam" value="boom"/>
          <input type="submit" class="button" value="POST myIntParam=boom"/>
        </form>
        </div>
      </x:frag>;
  }
}
