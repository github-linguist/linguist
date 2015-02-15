(use 'clojure.data.json)

 ; Load as Clojure data structures and bind the resulting structure to 'json-map'.
(def json-map (read-json "{ \"foo\": 1, \"bar\": [10, \"apples\"] }"))

; Use pr-str to print out the Clojure representation of the JSON created by read-json.
(pr-str json-map)

; Pretty-print the Clojure representation of JSON. We've come full circle.
(pprint-json json-map)
