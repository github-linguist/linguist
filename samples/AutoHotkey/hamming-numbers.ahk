SetBatchLines, -1
Msgbox % hamming(1,20)
Msgbox % hamming(1690)
return

hamming(first,last=0)
{
	if (first < 1)
		ans=ERROR

	if (last = 0)
		last := first

	i:=0, j:=0, k:=0

	num1 := ceil((last * 20)**(1/3))
	num2 := ceil(num1 * ln(2)/ln(3))
	num3 := ceil(num1 * ln(2)/ln(5))

	loop
	{
		H := (2**i) * (3**j) * (5**k)
		if (H > 0)
			ans = %H%`n%ans%
		i++
		if (i > num1)
		{
			i=0
			j++
			if (j > num2)
			{
				j=0
				k++
			}
		}
		if (k > num3)
			break
	}
	Sort ans, N

	Loop, parse, ans, `n, `r
	{
		if (A_index > last)
			break
		if (A_index < first)
			continue
		Output = %Output%`n%A_LoopField%
	}

	return Output
}
