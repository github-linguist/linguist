// 32-bit builds of PHP: Integers can be from -2147483648 to 2147483647
// 64-bit builds of PHP: Integers can be from -9223372036854775808 to 9223372036854775807

function middlethree($integer)
{
	$int 	= (int)str_replace('-','',$integer);
	$length = strlen($int);

	if(is_int($int))
	{
		if($length >= 3)
		{
			if($length % 2 == 1)
			{
				$middle = floor($length / 2) - 1;
				return substr($int,$middle, 3);
			}
			else
			{
				return 'The value must contain an odd amount of digits...';	
			}
		}
		else
		{
			return 'The value must contain at least three digits...';	
		}
	}
	else
	{
		return 'The value does not appear to be an integer...';
	}
}

$numbers = array(123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0);

foreach($numbers as $nums)
{
	echo $nums.' : '.middlethree($nums). '<br>';
}
