<?php
for($i=2008; $i<2121; $i++)
{
  $datetime = new DateTime("$i-12-25 00:00:00");
  if ( $datetime->format("w") == 0 )
  {
     echo "25 Dec $i is Sunday\n";
  }
}
?>
