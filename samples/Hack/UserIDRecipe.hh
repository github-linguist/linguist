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

class UserIDRecipe extends Recipe implements RecipeWithDemo {

  protected function getName(): string {
    return 'User ID';
  }

  <<Override>>
  protected function getDescription(): ?string {
    return 'Protect your user IDs from being confused with normal ints';
  }

  protected function getFilenames(): Vector<string> {
    return Vector {
      'UserID.php',
      'UsingUserID.php',
    };
  }

  protected function getDocs(): Vector<(string, string)> {
    return Vector {
      tuple('Opaque Type Aliasing',  'hack.typealiasing.opaquetypealiasing'),
      tuple(
        'Opaque Type Aliasing with Constraints',
        'hack.typealiasing.opaquewithconstraints',
      ),
    };
  }

  public function getDemoFilename(): string {
    return 'demo.php';
  }

  public function getDemoResult(): string {
    return user_id_main();
  }

  public function getDemoXHP(): ?:xhp {
    return null;
  }
}
