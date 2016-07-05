function isLeapYear ($year)
{
    If (([System.Int32]::TryParse($year, [ref]0)) -and ($year -le 9999))
	{
		$bool = [datetime]::isleapyear($year)
	}
	else
	{
		throw "Year format invalid. Use only numbers up to 9999."
	}
	return $bool
}
