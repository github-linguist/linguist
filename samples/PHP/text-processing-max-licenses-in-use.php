$handle = fopen ("mlijobs.txt", "rb");
$maxcount = 0;
$count = 0;
$times = array();
while (!feof($handle)) {
    $buffer = fgets($handle);
    $op = trim(substr($buffer,8,3));
	switch ($op){
		case 'IN':
			$count--;
		break;
		case 'OUT':
			$count++;
			preg_match('/([\d|\/|_|:]+)/',$buffer,$time);
			if($count>$maxcount){
				$maxcount = $count;
				$times = Array($time[0]);
			}elseif($count == $maxcount){
				$times[] = $time[0];
			}
		break;
	}	
}
fclose ($handle);

echo $maxcount . '<br>';
for($i=0;$i<count($times);$i++){
	echo $times[$i] . '<br>';
}
