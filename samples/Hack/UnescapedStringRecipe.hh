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

class UnescapedStringRecipe extends Recipe implements RecipeWithDemo {

  protected function getName(): string {
    return 'Unescaped string';
  }

  <<Override>>
  protected function getDescription(): ?string {
    return 'Forgetting to properly escape the strings you get from your users '.
      'can lead to serious security holes. Hack can help by forcing you to '.
      'escape these strings before using them as strings.';
  }

  protected function getFilenames(): Vector<string> {
    return Vector {
      'UnescapedString.php',
      'MySecureRequest.php',
    };
  }

  protected function getDocs(): Vector<(string, string)> {
    return Vector{
      tuple('Opaque Type Aliasing',  'hack.typealiasing.opaquetypealiasing'),
    };
  }

  public function getDemoFilename(): string {
    return 'demo.php';
  }

  public function getDemoResult(): string {
    return unescaped_string_main();
  }

  public function getDemoXHP(): ?:xhp {
    $url = '/recipes/unescaped-string/';
    return
      <x:frag>
        Try setting the myStrParam GET param to something nice and innocent with this button...
        <div>
          <a href={"$url?myStrParam='); DROP TABLE important_stuff; --#demo"} class="button">GET myStrParam=Hello world</a>
        </div>
      </x:frag>;
  }
}
