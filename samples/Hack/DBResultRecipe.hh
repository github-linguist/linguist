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

class DBResultRecipe extends Recipe implements RecipeWithDemo {

  protected function getName(): string {
    return 'DB Result';
  }

  <<Override>>
  protected function getDescription(): ?string {
    return 'Fetching data from a DB introduces a few typing challenges. '.
      'First, the data comes back untyped. Second, a row in a DB generally '.
      'contains columns of different types.';
  }

  protected function getFilenames(): Vector<string> {
    return Vector {
      'FakeDB.php',
    };
  }

  protected function getDocs(): Vector<(string, string)> {
    return Vector{
      tuple ('Hack Shapes', 'hack.shapes'),
      tuple ('Mixed Types', 'hack.annotations.mixedtypes'),
    };
  }

  public function getDemoFilename(): string {
    return 'demo.php';
  }

  public function getDemoResult(): string {
    return db_result_main();
  }

  public function getDemoXHP(): ?:xhp {
    return null;
  }
}
