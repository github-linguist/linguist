NewMap dict.s()
dict("de") = "German"
dict("en") = "English"
dict("fr") = "French"

ForEach dict()
  Debug MapKey(dict()) + ":" + dict()
Next
