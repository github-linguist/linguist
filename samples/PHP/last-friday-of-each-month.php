<?php
function last_friday_of_month($year, $month) {
  $day = 0;
  while(True) {
    $last_day = mktime(0, 0, 0, $month+1, $day, $year);
    if (date("w", $last_day) == 5) {
      return date("Y-m-d", $last_day);
    }
    $day -= 1;
  }
}

function print_last_fridays_of_month($year) {
  foreach(range(1, 12) as $month) {
    echo last_friday_of_month($year, $month), "<br>";
  }
}

date_default_timezone_set("GMT");
$year = 2012;
print_last_fridays_of_month($year);
?>
