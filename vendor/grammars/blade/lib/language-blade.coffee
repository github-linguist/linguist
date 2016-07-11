module.exports =
  config:
    useBladeComments:
      type: 'boolean'
      default: true
      description: 'Use Blade comments by default when toggling line comments'

  activate: (state) ->
    @changeUseBladeComments = atom.config.observe 'language-blade.useBladeComments', @setBladeComments

  deactivate: ->
    @changeUseBladeComments.dispose()

  setBladeComments: (enabled) ->
    opts = scopeSelector: ['.text.html.php.blade'];
    if enabled
      atom.config.set('editor.commentStart', '{{-- ', opts);
      atom.config.set('editor.commentEnd', ' --}}', opts);
    else
      atom.config.unset('editor.commentStart', opts);
      atom.config.unset('editor.commentEnd', opts);
