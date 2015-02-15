function cocktailSort($arr){
	if (is_string($arr)) $arr = str_split(preg_replace('/\s+/','',$arr));

	do{
		$swapped = false;
		for($i=0;$i<count($arr);$i++){
			if(isset($arr[$i+1])){
				if($arr[$i] > $arr[$i+1]){
					list($arr[$i], $arr[$i+1]) = array($arr[$i+1], $arr[$i]);
					$swapped = true;
				}
			}			
		}
		
		if ($swapped == false) break;
		
		$swapped = false;
		for($i=count($arr)-1;$i>=0;$i--){
			if(isset($arr[$i-1])){
				if($arr[$i] < $arr[$i-1]) {
					list($arr[$i],$arr[$i-1]) = array($arr[$i-1],$arr[$i]);
					$swapped = true;
				}
			}
		}
	}while($swapped);

	return $arr;
}
$arr = array(5, 1, 7, 3, 6, 4, 2);
$arr2 = array("John", "Kate", "Alice", "Joe", "Jane");
$strg = "big fjords vex quick waltz nymph";
$arr = cocktailSort($arr);
$arr2 = cocktailSort($arr2);
$strg = cocktailSort($strg);
echo implode(',',$arr) . '<br>';
echo implode(',',$arr2) . '<br>';
echo implode('',$strg) . '<br>';
