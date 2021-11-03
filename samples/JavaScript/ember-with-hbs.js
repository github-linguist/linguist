import { hbs } from 'any-ember-import';

const MyOption = hbs`
  <option selected={{@selected}} value={{@value}}>
    {{or @title @value}}
  </option>
`;

export const CustomSelect = hbs`
  <select>
    {{#each @options as |option|}}
      <MyOption
        @value={{option.value}}
        @selected={{eq option @selectedOption}}
      />
    {{/each}}
  </select>
`;
