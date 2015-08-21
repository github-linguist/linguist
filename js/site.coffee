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

  $.get "https://rawgit.com/github/linguist/master/lib/linguist/languages.yml", (data) ->
    languages = jsyaml.safeLoad(data)

    for language, attrs of languages
      attrs.name = language
      attrs.octicon = octiconMapping[attrs.type]
      $list.append(Mustache.render(template, attrs))
