Pixi Test
=========

Testing out Pixi.js

    _ = require "./lib/underscore"

    {applyStylesheet} = require "util"
    applyStylesheet require("./style")
    {width, height} = require "./pixie"

    {update, applyProperties, hydrate} = require "./object_updater"
    editor = require("./editor")()

    PIXI = require "./lib/pixi"
    # PIXI.Texture.SCALE_MODE.DEFAULT = PIXI.Texture.SCALE_MODE.NEAREST

    stage = new PIXI.Stage(0x66FF99)

    renderer = PIXI.autoDetectRenderer(width, height)

    clickHandler = (mouseData) ->
      if mouseData.originalEvent.ctrlKey or mouseData.originalEvent.metaKey
        editor.activeObject mouseData.target.data
      else
        if data = mouseData.target.data
          data.click?(data)

    document.body.appendChild(renderer.view)

Load textures from a data file and map them into Pixi.js texture objects

    textures = require "./textures"
    Object.keys(textures).forEach (name) ->
      value = textures[name]
      textures[name] = PIXI.Texture.fromImage("http://a0.pixiecdn.com/#{value}", true)

Reload our app data or use our default data.

    if data = ENV?.APP_STATE
      data = JSON.parse(data)
    else
      data = require("./default_data")

Fill children

    populate = (object) ->
      [0..4].forEach (i) ->
        [0..4].forEach (j) ->
          c = new PIXI.Sprite(textures.pixie)
          object.addChild c
          c.position =
            x: i * 50 - 125
            y: j * 50 - 125

Reconstitute our objects using our app data.

    objects = data.map (datum) ->
      object = new PIXI.Sprite(textures[datum.sprite])

      datum._host = object

      object.anchor.x = object.anchor.y = 0.5

      object.data = datum
      object.interactive = true
      object.click = clickHandler

      # TODO: Figure out child object compositions
      # something like
      # object.data.children?.forEach (datum) ->
      #   recurse object, datum
      # populate object

      hydrate(object.data)
      applyProperties(object)

      # TODO: stage should be 'parent' or 'root' so we can handle trees of objects
      stage.addChild(object)

      return object

Our main loop, update and draw.

    dt = 1/60
    animate = ->
      requestAnimationFrame(animate)

      objects.forEach (object) ->
        update(object, dt)

      renderer.render(stage)

    requestAnimationFrame(animate)

This is where we export and expose our app state.

    global.appData = ->
      JSON.stringify stage.children.map (child) ->
        _.omit child.data, "_host"

Text sample (still need to figure out how to handle different PIXI types for our
objects)

    addText = ->
      textSample = new PIXI.Text "Pixi.js can has\nmultiline text!",
        font: "35px Snippet"
        fill: "white"
        align: "left"
      textSample.position.x = 20
      textSample.position.y = 20
      stage.addChild(textSample)

