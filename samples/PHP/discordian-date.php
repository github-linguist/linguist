<?php
// Discordian dating machine. Accepts date from PHP function date() or from the URL.
// The Discordian calendar has 5 days in a week, 73 days in a month and 5 months in a year.
// Ex. ddate.php will generate todays date according to your computer
// Ex. ddate.php?y=2012&m=2&y=29 will generate the date for Feb. 29, 2012

$DAYS = array("Sweetmorn","Boomtime","Pungenday","Prickle-Prickle","Setting Orange");
$MONTHS = array("Chaos","Discord","Confusion","Bureaucracy","The Aftermath");

// If user passed year, month and day variables in URL
if(isset($_GET['y']) && isset($_GET['m']) && isset($_GET['d'])) {
	// Get year, month and day from URL
	$usery = $_GET['y']; $userm = $_GET['m']; $userd = $_GET['d'];
	// Use unix time to calculate day of year starting at zero
	$userdate = mktime(0,0,0,$userm,$userd,$usery);
	$ddays = ($userdate - mktime(0,0,0,1,1,$usery)) / 86400;
	// Calculates the Discordian year, month and day
	$dyear = ($usery) + 1166; $dmonth = $MONTHS[$dday/73]; $dday = $ddays%73 + 1;
}
// If user didn't pass year, get date from PHP function
else {
	// Create array containing current Gregorian year, month, day,
	// days of year starting at zero and whether or not year is a leap
	$date = explode(" ",date('Y n j z L'));
	// Calculates the Discordian year, month and day
	$dyear = $date[0]+1166;	$dmonth = $MONTHS[$date[3]/73];	$dday = $date[3]%73 +1;
}
// Determine the name of the day
if ($ddays === NULL) { $ddayname= $DAYS[$date[3]%5]; }
else { $ddayname= $DAYS[$ddays%5]; }

// Leap year exception for date from PHP
if ($ddays === NULL) {
	if ($date[4]) {
		if ($date[1] == 2 && $date[2] == 29) { echo "Today is St. Tib's Day, YOLD " . $dyear; }
		if ($date[3] >= 60) { //offset day by one if leap year
			$dday -= 1;
			if($dday % 5 == 0) $ddayname = $DAYS[4];
			else $ddayname = $DAYS[($dday%5)];
		}
	}
	// Display Discordian date
	echo "Today is " . $ddayname . ", " . $dmonth . " " . $dday . ", YOLD " . $dyear;
}
else { // Leap year exception for date from URL
	if ($usery % 100 == 0) { // leap year exception for years that are divisible by one hundred
		if ($usery % 400 == 0 && $userm == 2 && $userd == 29) { echo "Today is St. Tib's Day, YOLD " . $dyear; }
	}
	else if ($usery % 4 == 0) { // if the year is a leap year
 		if ($userm == 2 && $userd == 29) { echo "Today is St. Tib's Day, YOLD " . $dyear; }
		elseif ($ddays >= 60) { // offset day by one if leap year
			$dday -=1;
			if($dday % 5 == 0) $ddayname = $DAYS[4];
			else $ddayname=$DAYS[($dday%5-1)];
			echo "Today is " . $ddayname . ", " . $dmonth . " " . $dday . ", YOLD " . $dyear;
		}
		else { 	echo "Today is " . $ddayname . ", " . $dmonth . " " . $dday . ", YOLD " . $dyear; }
	}
	// Display Discordian date
	else { 	echo "Today is " . $ddayname . ", " . $dmonth . " " . $dday . ", YOLD " . $dyear; }
}
?>
