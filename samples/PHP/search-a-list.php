$haystack = array("Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo");

foreach (array("Washington","Bush") as $needle) {
  $i = array_search($needle, $haystack);
  if ($i === FALSE) // note: 0 is also considered false in PHP, so you need to specifically check for FALSE
    echo "$needle is not in haystack\n";
  else
    echo "$i $needle\n";
}
