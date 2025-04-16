%dw 2.0
---
{
  "boolean":{
    "true" : true,
    "false": false
  },
  "Number": {
    "int": 123,
    "decimal": 123.23
  },
  "string": {
    "singleQuote" : 'A String',
    "doubleQuote" : "A String"
  },
  "regex": /foo/,
  "date": {
    a: |2003-10-01|,
    b: |2005-045|,
    c: |2003-W14-3|,
    d: |23:57:59|,
    e: |23:57:30.700|,
    f: |23:50:30Z|,
    g: |+13:00|,
    h: |Z|,
    i: |-02:00|,
    j: |2005-06-02T15:10:16|,
    k: |2005-06-02T15:10:16Z|,
    l: |2005-06-02T15:10:16+03:00|,
    m: |P12Y7M11D|,
    n: |P12Y5M|,
    o: |P45DT9H20M8S|,
    p: |PT9H20M8S|
  }
}

