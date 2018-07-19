function quicksort($arr){
	$loe = $gt = array();
	if(count($arr) < 2){
		return $arr;
	}
	$pivot_key = key($arr);
	$pivot = array_shift($arr);
	foreach($arr as $val){
		if($val <= $pivot){
			$loe[] = $val;
		}elseif ($val > $pivot){
			$gt[] = $val;
		}
	}
	return array_merge(quicksort($loe),array($pivot_key=>$pivot),quicksort($gt));
}

$arr = array(1, 3, 5, 7, 9, 8, 6, 4, 2);
$arr = quicksort($arr);
echo implode(',',$arr);
