<?php
/**
 * @author Elad Yosifon
 */

/**
 * @param int $x
 * @param array $series
 * @param int $n
 * @return array
 */
function fib_n_step($x, &$series = array(1, 1), $n = 15)
{
	$count = count($series);

	if($count > $x && $count == $n) // exit point
	{
		return $series;
	}

	if($count < $n)
	{
		if($count >= $x) // 4 or less
		{
			fib($series, $x, $count);
			return fib_n_step($x, $series, $n);
		}
		else // 5 or more
		{
			while(count($series) < $x )
			{
				$count = count($series);
				fib($series, $count, $count);
			}
			return fib_n_step($x, $series, $n);
		}
	}

	return $series;
}

/**
 * @param array $series
 * @param int $n
 * @param int $i
 */
function fib(&$series, $n, $i)
{
	$end = 0;
	for($j = $n; $j > 0; $j--)
	{
		$end += $series[$i-$j];
	}
	$series[$i] = $end;
}


/*===================  OUTPUT ============================*/

header('Content-Type: text/plain');
$steps = array(
	'LUCAS' => 		array(2, 	array(2, 1)),
	'FIBONACCI' => 	array(2, 	array(1, 1)),
	'TRIBONACCI' =>	array(3, 	array(1, 1, 2)),
	'TETRANACCI' =>	array(4, 	array(1, 1, 2, 4)),
	'PENTANACCI' =>	array(5,	array(1, 1, 2, 4)),
	'HEXANACCI' =>	array(6, 	array(1, 1, 2, 4)),
	'HEPTANACCI' =>	array(7,	array(1, 1, 2, 4)),
	'OCTONACCI' =>	array(8, 	array(1, 1, 2, 4)),
	'NONANACCI' =>	array(9, 	array(1, 1, 2, 4)),
	'DECANACCI' =>	array(10, 	array(1, 1, 2, 4)),
);

foreach($steps as $name=>$pair)
{
	$ser = fib_n_step($pair[0],$pair[1]);
	$n = count($ser)-1;

	echo $name." => ".implode(',', $ser) . "\n";
}
