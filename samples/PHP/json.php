<?php
$data = json_decode('{ "foo": 1, "bar": [10, "apples"] }'); // dictionaries will be returned as objects
$data2 = json_decode('{ "foo": 1, "bar": [10, "apples"] }', true); // dictionaries will be returned as arrays

$sample = array( "blue" => array(1,2), "ocean" => "water" );
$json_string = json_encode($sample);
?>
