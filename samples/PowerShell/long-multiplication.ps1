# LongAddition only supports Unsigned Integers represented as Strings/Character Arrays
Function LongAddition ( [Char[]] $lhs, [Char[]] $rhs )
{
	$lhsl = $lhs.length
	$rhsl = $rhs.length
	if(($lhsl -gt 0) -and ($rhsl -gt 0))
	{
		$maxplace = [Math]::Max($rhsl,$lhsl)+1
		1..$maxplace | ForEach-Object {
			$carry = 0
			$result = ""
		} {
			$add1 = 0
			$add2 = 0
			if( $_ -le $lhsl ) { $add1 = [int]$lhs[ -$_ ] - 48 }
			if( $_ -le $rhsl ) { $add2 = [int]$rhs[ -$_ ] - 48 }
			$iresult = $add1 + $add2 + $carry
			if( ( $_ -lt $maxplace ) -or ( $iresult -gt 0 ) )
			{
				$result = "{0}{1}" -f ( $iresult % 10 ),$result
			}
			$carry = [Math]::Floor( $iresult / 10 )
		} {
			$result
		}
	} elseif($lhsl -gt 0) {
		[String]::Join( '', $lhs )
	} elseif($rhsl -gt 0) {
		[String]::Join( '', $rhs )
	} else {
		"0"
	}
}

# LongMultiplication only supports Unsigned Integers represented as Strings/Character Arrays
Function LongMultiplication ( [Char[]] $lhs, [Char[]] $rhs )
{
	$lhsl = $lhs.length
	$rhsl = $rhs.length
	if(($lhsl -gt 0) -and ($rhsl -gt 0))
	{
		1..$lhsl | ForEach-Object {
			$carry0 = ""
			$result0 = ""
		} {
			$i = -$_
			$add1 = ( 1..$rhsl | ForEach-Object {
				$carry1 = 0
				$result1 = ""
			} {
				$j = -$_
				$mult1 = [int]$lhs[ $i ] - 48
				$mult2 = [int]$rhs[ $j ] - 48
				$iresult1 = $mult1 * $mult2 + $carry1
				$result1 = "{0}{1}" -f ( $iresult1 % 10 ), $result1
				$carry1 = [Math]::Floor( $iresult1 / 10 )
			} {
				if( $carry1 -gt 0 )
				{
					$result1 = "{0}{1}" -f $carry1, $result1
				}
				$result1
			} )
			$iresult0 = ( LongAddition $add1 $carry0 )
			$iresultl = $iresult0.length
			$result0 = "{0}{1}" -f $iresult0[-1],$result0
			if( $iresultl -gt 1 ) {
				$carry0 = [String]::Join( '', $iresult0[ -$iresultl..-2 ] )
			} else { $carry0 = "" }
		} {
			if( $carry0 -ne "" )
			{
				$result0 = "{0}{1}" -f $carry0, $result0
			}
			$result0
		}
	} else { "0" }
}

LongMultiplication "18446744073709551616" "18446744073709551616"
