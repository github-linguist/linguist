<?php
$arr = array(-7, 1, 5, 2, -4, 3, 0);

function getEquilibriums($arr) {
    $right = array_sum($arr);
    $left = 0;
    $equilibriums = array();
    foreach($arr as $key => $value){
        $right -= $value;
        if($left == $right) $equilibriums[] = $key;
        $left += $value;
    }
    return $equilibriums;
}

echo "# results:\n";
foreach (getEquilibriums($arr) as $r) echo "$r, ";
?>
