function hailstone($n,$seq=array()){
	$sequence = $seq;
	$sequence[] = $n;
	if($n == 1){
		return $sequence;
	}else{
		$n = ($n%2==0) ? $n/2 : (3*$n)+1;
		return hailstone($n, $sequence);
	}
}

$result = hailstone(27);
echo count($result) . ' Elements.<br>';
echo 'Starting with : ' . implode(",",array_slice($result,0,4)) .' and ending with : ' . implode(",",array_slice($result,count($result)-4)) . '<br>';

$maxResult = array(0);

for($i=1;$i<=100000;$i++){
		$result = count(hailstone($i));
		if($result > max($maxResult)){
			$maxResult = array($i=>$result);
		}
}
foreach($maxResult as $key => $val){
echo 'Number < 100000 with longest Hailstone seq.: ' . $key . ' with length of ' . $val;
}
