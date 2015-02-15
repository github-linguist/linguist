declare
  [JSON] = {Module.link ['JSON.ozf']}

  {System.show {JSON.decode "{ \"foo\": 1, \"bar\": [10, \"apples\"] }"}}

  Sample = object(blue:array(1 2) ocean:"water")
  {System.showInfo {JSON.encode Sample}}
