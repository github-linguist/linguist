; Create an associative array
obj := Object("red", 0xFF0000, "blue", 0x0000FF, "green", 0x00FF00)
enum := obj._NewEnum()
While enum[key, value]
    t .= key "=" value "`n"
MsgBox % t
