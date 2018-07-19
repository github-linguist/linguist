function ZigZagMatrix($num) {
    $matrix = array();
    for ($i = 0; $i < $num; $i++){
		$matrix[$i] = array();
	}
	
    $i=1;
	$j=1;
    for ($e = 0; $e < $num*$num; $e++) {
        $matrix[$i-1][$j-1] = $e;
        if (($i + $j) % 2 == 0) {
            if ($j < $num){
				$j++;
			}else{
				$i += 2;
			}
            if ($i > 1){
				$i --;
			}
        } else {
            if ($i < $num){
				$i++;
			}else{
				$j += 2;
			}
            if ($j > 1){
				$j --;
			}
        }
    }
	return $matrix;
}
