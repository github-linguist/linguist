require 'json'
ruby_obj = JSON.parse('{"blue": [1, 2], "ocean": "water"}')
p ruby_obj
puts ruby_obj.class
puts ruby_obj["blue"].class
ruby_obj["ocean"] = {"water" => %w{fishy salty}}
puts JSON.generate(ruby_obj)
