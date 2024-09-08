import Component from '@glimmer/component';
import FreestyleUsage from 'ember-freestyle/components/freestyle/usage';
import BoxelInputTime, { Time } from './index';
import { tracked } from '@glimmer/tracking';
import { cssVariable, CSSVariableInfo } from 'ember-freestyle/decorators/css-variable';
import { fn } from '@ember/helper';
import { action } from '@ember/object';

export default class BoxelInputTimeUsage extends Component {
  cssClassName = 'boxel-input-time';

  @cssVariable declare boxelInputTimeBackgroundColor: CSSVariableInfo; // TODO: replace or remove
  @tracked value = new Date(2022,2,3,13,45);
  @tracked minValue = new Date(2022,2,3,11,0);
  @tracked minuteInterval = 5;
  @action timeChanged(val: Time) {
    this.value = val as Date; //TODO: casting???
  }
  <template>
    <FreestyleUsage @name="Input::Time">
      <:description>
        A succint version of a time picker.
      </:description>
      <:example>
        <BoxelInputTime
          @value={{this.value}}
          @minValue={{this.minValue}}
          @minuteInterval={{this.minuteInterval}}
          @onChange={{this.timeChanged}}
        />
      </:example>
      <:api as |Args|>
        <Args.Object
          @name="value"
          @description="The current value (undefined or conforming to a Time interface that is a subset of JavaScript's Date API"
          @value={{this.value}}
          @onInput={{fn (mut this.value)}}
        />
        <Args.Object
          @name="minValue"
          @description="The times before this value will disabled"
          @value={{this.minValue}}
        />
        <Args.Number
          @name="minuteInterval"
          @description="The interval at which to show minute options"
          @defaultValue={{5}}
          @value={{this.minuteInterval}}
          @onInput={{fn (mut this.minuteInterval)}}
        />
        <Args.Action
          @name="onChange"
          @description="Called when the user changed the time"
        />
      </:api>
    </FreestyleUsage>
  </template>
}