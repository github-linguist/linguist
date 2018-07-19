<?php
/**
 * @author Elad Yosifon
 */
$roman_to_decimal = array(
	'I' => 1,
	'V' => 5,
	'X' => 10,
	'L' => 50,
	'C' => 100,
	'D' => 500,
	'M' => 1000,
);

/**
 * @param $number
 * @return int
 */
function roman2decimal($number)
{
	global $roman_to_decimal;

	// breaks the string into an array of chars
	$digits = str_split($number);
	$lastIndex = count($digits)-1;
	$sum = 0;

	foreach($digits as $index => $digit)
	{
		if(!isset($digits[$index]))
		{
			continue;
		}

		if(isset($roman_to_decimal[$digit]))
		{
			if($index < $lastIndex)
			{
				$left = $roman_to_decimal[$digits[$index]];
				$right = $roman_to_decimal[$digits[$index+1]];
				if($left < $right)
				{
					$sum += ($right - $left);
					unset($digits[$index+1],$left, $right);
					continue;
				}
				unset($left, $right);
			}
		}
		$sum += $roman_to_decimal[$digit];
	}

	return $sum;
}

/*============= OUTPUT =============*/
header('Content-Type: text/plain');

$tests = array(
	"I" => array(roman2decimal('I'), 1),
	"II" => array(roman2decimal('II'), 2),
	"III" => array(roman2decimal('III'), 3),
	"IV" => array(roman2decimal('IV'), 4),
	"V" => array(roman2decimal('V'), 5),
	"VI" => array(roman2decimal('VI'), 6),
	"VII" => array(roman2decimal('VII'), 7),
	"IX" => array(roman2decimal('IX'), 9),
	"X" => array(roman2decimal('X'), 10),
	"XI" => array(roman2decimal('XI'), 11),
	"XIV" => array(roman2decimal('XIV'), 14),
	"XV" => array(roman2decimal('XV'), 15),
	"XVI" => array(roman2decimal('XVI'), 16),
	"XVIV" => array(roman2decimal('XVIV'), 19),
	"XIX" => array(roman2decimal('XIX'), 19),
	"MDCLXVI" => array(roman2decimal('MDCLXVI'), 1666),
	"MCMXC" => array(roman2decimal('MCMXC'), 1990),
	"MMVIII" => array(roman2decimal('MMVIII'), 2008),
	"MMMCLIX" => array(roman2decimal('MMMCLIX'), 3159),
	"MCMLXXVII" => array(roman2decimal('MCMLXXVII'), 1977),
);


foreach($tests as $key => $value)
{
	echo "($key == {$value[0]}) => " . ($value[0] === $value[1] ? "true" : "false, should be {$value[1]}.") . "\n";
}
