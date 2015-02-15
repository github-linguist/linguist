<?php
$pairs = array( "hello" => 1,
		"world" => 2,
		"!"     => 3 );

// iterate over key-value pairs
foreach($pairs as $k => $v) {
  echo "(k,v) = ($k, $v)\n";
}

// iterate over keys
foreach(array_keys($pairs) as $key) {
  echo "key = $key, value = $pairs[$key]\n";
}

// iterate over values
foreach($pairs as $value) {
  echo "values = $value\n";
}
?>
