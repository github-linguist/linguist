<?php
$finalres = Array();
function permut($arr,$result=array()){
	global  $finalres;
	if(empty($arr)){
		$finalres[] = implode("",$result);
	}else{
		foreach($arr as $key => $val){
			$newArr = $arr;
			$newres = $result;
			$newres[] = $val;
			unset($newArr[$key]);
			permut($newArr,$newres);		
		}
	}
}
$givenPerms = Array("ABCD","CABD","ACDB","DACB","BCDA","ACBD","ADCB","CDAB","DABC","BCAD","CADB","CDBA","CBAD","ABDC","ADBC","BDCA","DCBA","BACD","BADC","BDAC","CBDA","DBCA","DCAB");
$given = Array("A","B","C","D");
permut($given);
print_r(array_diff($finalres,$givenPerms)); // Array ( [20] => DBAC )
