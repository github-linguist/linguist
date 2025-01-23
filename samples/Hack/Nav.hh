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

type NavItem = shape(
  'name' => string,
  'location' => string,
);

type NavSection = shape(
  'name' => string,
  'location' => ?string,
  'items' => Vector<NavItem>,
);

final class :hack:nav extends :x:element {
  private function getNavSections(): Vector<NavSection> {
    return Vector{
      shape(
        'name' => 'Home',
        'location' => '/',
        'items' => Vector {},
      ),
      shape(
        'name' => 'GitHub',
        'location' => 'http://github.com/facebook/hack-example-site',
        'items' => Vector {},
      ),
      shape(
        'name' => 'Recipes',
        'location' => null,
        'items' => Vector {
          shape(
            'name' => '$_GET and $_POST',
            'location' => '/recipes/get-and-post/',
          ),
          shape(
            'name' => 'Assert',
            'location' => '/recipes/assert/',
          ),
          shape(
            'name' => 'DB Result',
            'location' => '/recipes/db-result/',
          ),
          shape(
            'name' => 'Unescaped String',
            'location' => '/recipes/unescaped-string/',
          ),
          shape(
            'name' => 'User ID',
            'location' => '/recipes/user-id/',
          ),
        },
      ),
    };
  }

  private function renderNavItems(Vector<NavItem> $items): :xhp {
    $render_item = $item ==>
      <li>
        <a class="navItem" href={$item['location']}>
          {$item['name']}
        </a>
      </li>;
    return
      <x:frag>
        {$items->map($render_item)->toArray()}
      </x:frag>;
  }

  private function renderNavSection(NavSection $section): :xhp {
    $section_item = <h3 class="navItem">{$section['name']}</h3>;
    if ($section['location'] !== null) {
      $section_item = <a href={$section['location']}>{$section_item}</a>;
    }
    return
      <li class="navSectionItem">
        {$section_item}
        <ul class="navItems">
          {$this->renderNavItems($section['items'])}
        </ul>
      </li>;
  }

  public function render(): :xhp {
    $sections = $this->getNavSections()
      ->map($section ==> $this->renderNavSection($section));
    return
      <div class="nav">
        <ul class="navSections">
          {$sections->toArray()}
        </ul>
      </div>;
  }
}
