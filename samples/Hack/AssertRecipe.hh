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

class AssertRecipe extends Recipe implements RecipeWithDemo {

  protected function getName(): string {
    return 'Assert';
  }

  <<Override>>
  protected function getDescription(): ?string {
    return 'When you have values with unknown types, it is useful to make '.
      'some runtime assertions and have the type checker understand. This '.
      'recipe demonstrates one approach.';
  }

  protected function getFilenames(): Vector<string> {
    return Vector {
      'Assert.php',
    };
  }

  protected function getDocs(): Vector<(string, string)> {
    return Vector{
      tuple ('Mixed Types', 'hack.annotations.mixedtypes'),
      tuple ('Type Inference', 'hack.otherrulesandfeatures.typeinference'),
    };
  }

  public function getDemoFilename(): string {
    return 'demo.php';
  }

  public function getDemoResult(): string {
    return assert_main();
  }

  public function getDemoXHP(): ?:xhp {
    return null;
  }
}
