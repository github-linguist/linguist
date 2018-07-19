<?php

/**
 * @author Elad Yosifon
 */

/**
 * @param int $binary
 * @return int
 */
function gray_encode($binary){
	return $binary ^ ($binary >> 1);
}

/**
 * @param int $gray
 * @return int
 */
function gray_decode($gray){
	$binary = $gray;
	while($gray >>= 1) $binary ^= $gray;
	return $binary;
}

for($i=0;$i<32;$i++){
	$gray_encoded = gray_encode($i);
	printf("%2d : %05b => %05b => %05b : %2d \n",$i, $i, $gray_encoded, $gray_encoded, gray_decode($gray_encoded));
}
