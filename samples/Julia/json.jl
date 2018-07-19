julia> import JSON

julia> JSON.parse("""{ "blue": [1,2], "ocean": "water" }""")
["ocean"=>"water","blue"=>{1,2}]

julia> JSON.json({"blue" => [1,2] , "ocean" => "water"})
"{\"ocean\":\"water\",\"blue\":[1,2]}"
