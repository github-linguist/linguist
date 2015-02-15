<?php

/**
 * @author Elad Yosifon
 */

/**
 * @param int $n
 * @param int $k
 * @return float|int
 */
function sum_of_a_series($n,$k)
{
	$sum_of_a_series = 0;
	for($i=$k;$i<=$n;$i++)
	{
		$sum_of_a_series += (1/($i*$i));
	}
	return $sum_of_a_series;
}

echo sum_of_a_series(1000,1);
