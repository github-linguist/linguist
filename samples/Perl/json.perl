use JSON;

my $data = decode_json('{ "foo": 1, "bar": [10, "apples"] }');

my $sample = { blue => [1,2], ocean => "water" };
my $json_string = encode_json($sample);
