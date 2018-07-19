<?php
$csv = <<<EOT
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
EOT;

function convert($csv)
{
  $out = [];
  array_map(function($ln) use(&$out) {
      $ln    = htmlentities($ln);
      $out[] = count($out) == 0
        ? '<thead><tr><th>'.implode('</th><th>',explode(',',$ln))."</th></tr></thead>\n"
        : '<tr><td>'.implode('</td><td>',explode(',',$ln))."</td></tr>\n";
    }, explode("\n",$csv));
  return '<table>'.implode('',$out).'</table>';
}

echo convert($csv);
