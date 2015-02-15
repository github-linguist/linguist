function combSort($arr){
	$gap = count($arr);
        $swap = true;
	while ($gap > 1 || $swap){
		if($gap > 1) $gap /= 1.25;
		
		$swap = false;
		$i = 0;
		while($i+$gap < count($arr)){
			if($arr[$i] > $arr[$i+$gap]){
				list($arr[$i], $arr[$i+$gap]) = array($arr[$i+$gap],$arr[$i]);
				$swap = true;
			}
			$i++;
		}
	}
	return $arr;
}
