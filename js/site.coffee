---
# Hey Jekyll, convert this please.
---

octiconMapping =
  programming: "file-binary"
  data: "database"
  markup: "file-code"
  prose: "file-text"

$ ->
  $list = $("#languages")
  template = $("#template").text()
  fuse = null

  $.get "https://rawgit.com/github/linguist/master/lib/linguist/languages.yml", (data) ->
    languages = []

    for language, attrs of jsyaml.safeLoad(data)
      attrs.name = language
      attrs.octicon = octiconMapping[attrs.type]
      languages.push attrs

      $list.append(Mustache.render(template, attrs))

    fuse = new Fuse languages,
      keys: ["name"],
      threshold: 0.4

  $('.js-search-field').keyup ->
    filter = $(this).val()

    if filter == ""
      $list.children().addClass('visible')
      return

    $list.children().removeClass('visible')

    for language in fuse.search(filter)
      $list.children('[data-language="' + language.name + '"]').addClass('visible')

  $('.js-autofocus').focus()
