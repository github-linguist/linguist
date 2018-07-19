<?php
$j2justtype = array('L' => STR_PAD_RIGHT,
                    'R' => STR_PAD_LEFT,
                    'C' => STR_PAD_BOTH);

/**
 Justify columns of textual tabular input where the record separator is the newline
 and the field separator is a 'dollar' character.
 justification can be L, R, or C; (Left, Right, or Centered).

 Return the justified output as a string
*/
function aligner($str, $justification = 'L') {
  global $j2justtype;
  assert(array_key_exists($justification, $j2justtype));
  $justtype = $j2justtype[$justification];

  $fieldsbyrow = array();
  foreach (explode("\n", $str) as $line)
    $fieldsbyrow[] = explode('$', $line);
  $maxfields = max(array_map('count', $fieldsbyrow));

  foreach (range(0, $maxfields-1) as $col) {
    $maxwidth = 0;
    foreach ($fieldsbyrow as $fields)
      $maxwidth = max($maxwidth, strlen($fields[$col]));
    foreach ($fieldsbyrow as &$fields)
      $fields[$col] = str_pad($fields[$col], $maxwidth, ' ', $justtype);
    unset($fields); // see http://bugs.php.net/29992
  }
  $result = '';
  foreach ($fieldsbyrow as $fields)
    $result .= implode(' ', $fields) . "\n";
  return $result;
}

$textinfile = 'Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$\'dollar\'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.';

foreach (array('L', 'R', 'C') as $j)
  echo aligner($textinfile, $j);

?>
