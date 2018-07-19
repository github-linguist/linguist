$data = Array(
			Array("Tyler Bennett","E10297",32000,"D101"),
			Array("John Rappl","E21437",47000,"D050"),
			Array("George Woltman","E00127",53500,"D101"),
			Array("Adam Smith","E63535",18000,"D202"),
			Array("Claire Buckman","E39876",27800,"D202"),
			Array("David McClellan","E04242",41500,"D101"),
			Array("Rich Holcomb","E01234",49500,"D202"),
			Array("Nathan Adams","E41298",21900,"D050"),
			Array("Richard Potter","E43128",15900,"D101"),
			Array("David Motsinger","E27002",19250,"D202"),
			Array("Tim Sampair","E03033",27000,"D101"),
			Array("Kim Arlich","E10001",57000,"D190"),
			Array("Timothy Grove","E16398",29900,"D190")
			);
function top_sal($num){
	global $data;
	$depts = Array();
	foreach($data as $key => $arr){
		if(!isset($depts[$arr[3]])) $depts[$arr[3]] = Array();
		$depts[$arr[3]][] = $key;
	}	
	function topsalsort($a,$b){
		global $data;
		if ($data[$a][2] == $data[$b][2]) {
			return 0;
		}
		return ($data[$a][2] < $data[$b][2]) ? 1 : -1;
	}
	foreach ($depts as $key => $val){
		usort($depts[$key],"topsalsort");
	}
	ksort($depts);
	echo '<pre>';
	foreach ($depts as $key => $val){
		echo $key . '<br>';
		echo 'Name			ID		Salary<br>';
		$count = 0;
		foreach($val as $value){
			echo $data[$value][0] . '	' . $data[$value][1] . '	' . $data[$value][2] . '<br>';
			$count++;
			if($count>=$num) break;
		}
		echo '<br>';
	}
	echo '</pre>';
}
top_sal(3);
