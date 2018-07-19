<?php
function montyhall($iterations){
	$switch_win = 0;
	$stay_win = 0;
		
	foreach (range(1, $iterations) as $i){
		$doors = array(0, 0, 0);
		$doors[array_rand($doors)] = 1;
		$choice = array_rand($doors);
		do {
			$shown = array_rand($doors);
		} while($shown == $choice || $doors[$shown] == 1);
			
		$stay_win += $doors[$choice];
		$switch_win += $doors[3 - $choice - $shown];
	}
		
	$stay_percentages = ($stay_win/$iterations)*100;
	$switch_percentages = ($switch_win/$iterations)*100;
		
	echo "Iterations: {$iterations} - ";
	echo "Stayed wins: {$stay_win} ({$stay_percentages}%) - ";
	echo "Switched wins: {$switch_win} ({$switch_percentages}%)";
}
	
        montyhall(10000);
?>
