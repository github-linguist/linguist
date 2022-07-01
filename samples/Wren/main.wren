// Lines - A small text editor

import "graphics" for Canvas, Color
import "font" for Font
import "dome" for Platform, Process, Window
import "input" for Keyboard
import "io" for FileSystem 

// var DefaultFont = "/usr/share/fonts/truetype/piboto/Piboto-Regular.ttf"
// var DefaultFont = "/usr/share/fonts/truetype/noto/NotoMono-Regular.ttf"
var DefaultFont = "UbuntuMono-R.ttf"
var FontSize = 16
var LineHeight = FontSize + 4
var BackgroundColor = Color.black

class Text {

  // https://rosettacode.org/wiki/Category_talk:Wren-fmt
  static rjust(s, w) {
    var c = s.count
    return (w > c) ? " " * (w - c) + s : s
  }

  gutterWidth {
    var area = Canvas.getPrintArea(_lines.count.max(10).toString)
    return area.x + 4
  }

  gutterPrint(lineNumber, y) {
    var w = _lines.count.max(10).toString.count
    Canvas.print(Text.rjust(lineNumber.toString, w), 2, y, Color.white)
  }

  count {
    return _lines.count
  }

  [index] {
    return _lines[index]
  }

  cursorX { 
    var lineLength = _lines[_y].count
    return _x.min(lineLength)
  }
  cursorX=(value) { _x = value }
  cursorY { _y }
  cursorY=(value) { _y = value }
  currentLine { _lines[_y] }
  currentLine=(value) { 
    _lines[_y] = value
    System.print("Ln(%(_y)): %(value)")
  }

  splitCurrentLine() {
    var line = currentLine
    var prefix = line[0...cursorX]
    var suffix = line[cursorX..-1]
    currentLine = prefix
    cursorY = cursorY + 1
    _lines.insert(cursorY, suffix)
    cursorX = 0  
  }

  joinCurrentLine() {
    var line = currentLine
    _lines.removeAt(cursorY)
    cursorY = cursorY - 1
    cursorX = currentLine.count
    currentLine = currentLine + line
  }

  visibleLinesCount { _visibleLines.count }

  construct new(text) {
    _lines = text.split("\n")
    _visibleLines = []
    _x = 0
    _y = 0
  }

  static load(fileName) {
    return Text.new(FileSystem.load(fileName))
  }

  save(fileName) {
    FileSystem.save(fileName, _lines.join("\n"))
  }

  // Split a single line into a list of short lines to fit into a given width
  static fit(line, width) {
    var area = Canvas.getPrintArea(line)
    
    if (area.x <= width) {
      return [ line ]
    }
    
    var split = (line.count * width / area.x).floor

    var lines = Text.fit(line[0...split], width)
    lines.addAll(Text.fit(line[split..-1], width))
    
    return lines
  }

  // Fit a list of lines 
  fit(topLineNumber, height) {
    _visibleLines = []

    var lineNumber = 0
    var textWidth = Canvas.width - gutterWidth - 4
    var linesCount = (height / LineHeight).floor.min(_lines.count)

    for (line in _lines[topLineNumber...(topLineNumber + linesCount)]) {
      var shortLines = Text.fit(line, textWidth)
      var visibleCount = (linesCount - lineNumber).min(shortLines.count)
      _visibleLines.add(shortLines[0...visibleCount])

      lineNumber = lineNumber + visibleCount
      if (lineNumber >= linesCount) {
        break
      }
    }

    //_y = _y.min(topLineNumber + _visibleLines.count - 1)

    System.print("Fit: %(topLineNumber) -> %(_visibleLines.count)")
  }

  print(topLineNumber, cursorOn) {
    var y = 6
    var lineNumber = topLineNumber
    var gw = gutterWidth

    for (line in _visibleLines) {
      gutterPrint(lineNumber + 1, y)

      var lineLength = _lines[lineNumber].count

      // If the cursor is further right than the total line length
      var realCursorX = cursorX

      var offsetX = 0
      var offsetY = 0
      for (shortLine in line) {
        Canvas.print(shortLine, gw + 4, y, Color.white)

        // Cursor can be placed within or at the end of the logical line
        if (cursorOn && lineNumber == cursorY && realCursorX >= offsetX && (realCursorX < offsetX + shortLine.count || offsetY == line.count - 1 && realCursorX == lineLength)) {  
          var end = realCursorX - offsetX
          //System.print("%(shortLine). len=%(shortLine.count) realX=%(realCursorX) offsX=%(offsetX) end=%(end)")
         
          var prefixArea = Canvas.getPrintArea(shortLine[0 ... end])
          Canvas.rectfill(gw + 2 + prefixArea.x, y - 2, 2, FontSize, Color.green)
        }

        y = y + LineHeight
        offsetY = offsetY + 1
        offsetX = offsetX + shortLine.count
      }
      lineNumber = lineNumber + 1
    }
  }
}

class Main {

  construct new() {}

  init() {
    Font.load("default", DefaultFont, FontSize)
    Font["default"].antialias = true
    Canvas.font = "default"

    var args = Process.args[2..-1]

    _fileName = args.count > 0 ? args[0] : "README.md"

    Window.title = "DomeEdit: %(_fileName)"
    //Window.lockstep = true

    Keyboard.handleText = true

    _text = Text.load(_fileName)
    _topLineNumber = 0
    _cursorOn = true
    _dirty = false
    _changes = false

    resize()
  }

  resize() {
    System.print("Resize -> %(Window.width), %(Window.height)")
    Canvas.resize(Window.width, Window.height, BackgroundColor)
    _statusY = Canvas.height - FontSize - 4

    _text.fit(_topLineNumber, _statusY)
  }

  save() {
    _text.save(_fileName)
    _dirty = false
  }

  update() {
    if (Canvas.width != Window.width || Canvas.height != Window.height) {
      resize()
    }
    _cursorOn = Platform.time % 2 == 1

    var line = _text.currentLine
    var cursorX = _text.cursorX 

    if (Keyboard.text.count > 0) {
      _text.currentLine = line[0...cursorX] + Keyboard.text + line[cursorX..-1]
      _text.cursorX = cursorX + Keyboard.text.count  
      _text.fit(_topLineNumber, _statusY)
      _dirty = true
    } else if (Keyboard["backspace"].justPressed) {
      if (_text.cursorX > 0) {
        _text.currentLine = line[0...(cursorX - 1)] + line[cursorX..-1]
        _text.cursorX = cursorX - 1  
      } else if (_text.cursorY > 0) {
        _text.joinCurrentLine()
      }
      _text.fit(_topLineNumber, _statusY)
    } else if (Keyboard["return"].justPressed) {
      _text.splitCurrentLine()
      _text.fit(_topLineNumber, _statusY)
      _dirty = true
    } else if (Keyboard["up"].justPressed) {
      _cursorOn = true
      if (_text.cursorY > 0) {
        _text.cursorY = _text.cursorY - 1
      }
      if (_text.cursorY < _topLineNumber) {
        _topLineNumber = _topLineNumber - 1
        _text.fit(_topLineNumber, _statusY)
      }
    } else if (Keyboard["down"].justPressed) {
      _cursorOn = true
      if (_text.cursorY < _text.count - 1) {
        _text.cursorY = _text.cursorY + 1
      }
      if (_text.cursorY >= _topLineNumber + _text.visibleLinesCount) {
        _topLineNumber = _topLineNumber + 1
        _text.fit(_topLineNumber, _statusY)
      }
    } else if (Keyboard["left"].justPressed) {
      _cursorOn = true
      if (_text.cursorX > 0) {
        _text.cursorX = _text.cursorX - 1
      }
    } else if (Keyboard["right"].justPressed) {
      _cursorOn = true
      if (_text.cursorX < _text.currentLine.count) {
        _text.cursorX = _text.cursorX + 1
      }
    } else if ((Keyboard["left ctrl"].down || Keyboard["right ctrl"].down) && Keyboard["s"].justPressed) {
      save()
    }
  }

  draw(alpha) {
    Canvas.cls(BackgroundColor)

    var gutterWidth = _text.gutterWidth

    // Status line
    Canvas.rectfill(0, _statusY, Canvas.width, FontSize + 4, Color.darkblue)
    Canvas.print("%(_fileName) %(_text.cursorY+1):%(_text.cursorX+1)", gutterWidth + 4, _statusY + 4, Color.white)
    if (_dirty) {
      Canvas.print("*", 6, _statusY + 6, Color.white)
    }

    // Gutter
    Canvas.rectfill(0, 0, gutterWidth, Canvas.height - FontSize - 4, Color.darkblue)

    _text.print(_topLineNumber, _cursorOn)
  }
}

var Game = Main.new()
