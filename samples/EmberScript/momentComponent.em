class App.FromNowView extends Ember.View
    tagName: 'time'
    template: Ember.Handlebars.compile '{{view.output}}'
    output: ~>
        return moment(@value).fromNow()

    didInsertElement: ->
        @tick()

    tick: ->
        f = ->
            @notifyPropertyChange 'output'
            @tick()

        nextTick = Ember.run.later(this, f, 1000)
        @set 'nextTick', nextTick

    willDestroyElement: ->
        nextTick = @nextTick
        Ember.run.cancel nextTick

Ember.Handlebars.helper 'fromNow', App.FromNowView

