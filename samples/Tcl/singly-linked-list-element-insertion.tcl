# Assume rest of definition is already present
oo::define List method insertAfter element {
    $element attach $next
    set next $element
}

set A [List new "A" [List new "B"]]
$A insertAfter [List new "C"]
