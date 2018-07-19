foo_hist = []
trace_var(:$foo){|v| foo_hist.unshift(v)}

$foo = "apple"
$foo = "pear"
$foo = "banana"

p foo_hist # => ["banana", "pear", "apple"]
