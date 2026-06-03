def test_interpolate: "abc \("nested \(. | ascii_upcase)")";

def nothing: empty;

def as_obj_var: . as {$a, $b} | $a;
def as_array_var: [1] as [$a] | $a;
def as_obj_array_var: {a: [1]} as {a: [$a, $b], $c} | $a;
def as_array_obj_var: [{a: 1}] as [{$a, a: $b}] | $a;

def array_in_object: {abc: [123, "abc", {abc: [123, 123]}]};

def f($var): $var;
