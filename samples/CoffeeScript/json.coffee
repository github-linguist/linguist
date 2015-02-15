sample =
  blue: [1, 2]
  ocean: 'water'

json_string = JSON.stringify sample
json_obj = JSON.parse json_string

console.log json_string
console.log json_obj
