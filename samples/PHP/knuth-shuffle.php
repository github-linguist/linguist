//The Fisher-Yates original Method
function yates_shuffle($arr){
	$shuffled = Array();
	while($arr){
		$rnd = array_rand($arr);
		$shuffled[] = $arr[$rnd];
		array_splice($arr, $rnd, 1);
	}
	return $shuffled;
}

//The modern Durstenfeld-Knuth algorithm
function knuth_shuffle(&$arr){
	for($i=count($arr)-1;$i>0;$i--){
		$rnd = mt_rand(0,$i);
		list($arr[$i], $arr[$rnd]) = array($arr[$rnd], $arr[$i]);
	}
}
