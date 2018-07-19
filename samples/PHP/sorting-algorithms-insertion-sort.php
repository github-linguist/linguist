function insertionSort(&$arr){
	for($i=0;$i<count($arr);$i++){
		$val = $arr[$i];
		$j = $i-1;
		while($j>=0 && $arr[$j] > $val){
			$arr[$j+1] = $arr[$j];
			$j--;
		}
		$arr[$j+1] = $val;
	}
}

$arr = array(4,2,1,6,9,3,8,7);
insertionSort($arr);
echo implode(',',$arr);
