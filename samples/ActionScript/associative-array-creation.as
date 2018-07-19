var map:Object = {key1: "value1", key2: "value2"};
trace(map['key1']); // outputs "value1"

// Dot notation can also be used
trace(map.key2); // outputs "value2"

// More keys and values can then be added
map['key3'] = "value3";
trace(map['key3']); // outputs "value3"
