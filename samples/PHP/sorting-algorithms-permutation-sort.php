function inOrder($arr){
	for($i=0;$i<count($arr);$i++){
		if(isset($arr[$i+1])){
			if($arr[$i] > $arr[$i+1]){
				return false;
			}
		}
	}
	return true;
}

function permute($items, $perms = array( )) {
    if (empty($items)) {
		if(inOrder($perms)){
			return $perms;
		}
    }  else {
        for ($i = count($items) - 1; $i >= 0; --$i) {
             $newitems = $items;
             $newperms = $perms;
             list($foo) = array_splice($newitems, $i, 1);
             array_unshift($newperms, $foo);
             $res = permute($newitems, $newperms);
			 if($res){
				return $res;
			 }		 		
         }
    }
}

$arr = array( 8, 3, 10, 6, 1, 9, 7, 2, 5, 4);
$arr = permute($arr);
echo implode(',',$arr);
