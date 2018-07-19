function pascalsTriangle($num){
	$c = 1;
	$triangle = Array();
	for($i=0;$i<=$num;$i++){
		$triangle[$i] = Array();
		if(!isset($triangle[$i-1])){
			$triangle[$i][] = $c;
		}else{
			for($j=0;$j<count($triangle[$i-1])+1;$j++){
				$triangle[$i][] = (isset($triangle[$i-1][$j-1]) && isset($triangle[$i-1][$j])) ? $triangle[$i-1][$j-1] + $triangle[$i-1][$j] : $c;
			}
		}
	}
	return $triangle;
}

$tria = pascalsTriangle(8);
foreach($tria as $val){
	foreach($val as $value){
		echo $value . ' ';
	}
	echo '<br>';
}
