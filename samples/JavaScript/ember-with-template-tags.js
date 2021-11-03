import { modifier } from 'ember-modifier';

const plusOne = (num) => num + 1;

const setScrollPosition = modifier((element, [position]) => {
  element.scrollTop = position
});

<template>
  <div
    class="scroll-container"
    {{setScrollPosition @scrollPos}}
  >
    {{#each @items as |item index|}}
      Item #{{plusOne index}}: {{item}}
    {{/each}}
  </div>
</template>


