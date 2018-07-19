function stoogeSort(&$arr, $i, $j)
{
	if($arr[$j] < $arr[$i])
	{
		list($arr[$j],$arr[$i]) = array($arr[$i], $arr[$j]);
	}
	if(($j - $i) > 1)
	{
		$t = ($j - $i + 1) / 3;
		stoogesort($arr, $i, $j - $t);
		stoogesort($arr, $i + $t, $j);
		stoogesort($arr, $i, $j - $t);
	}
}
